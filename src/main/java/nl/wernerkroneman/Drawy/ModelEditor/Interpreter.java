package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * The class that houses the most important, top-level parts
 * of the system that turns parsed English sentences commands
 * that the system can work with.
 * <p>
 * If the system sometimes looks a bit spaghetti-ish, please remember
 * that the language we're trying to understand is not actually meant
 * to be a good, simple and unambiguous language.
 */
public class Interpreter {

    private final PrepositionInterpreter prepositionInterpreter;
    KnowledgeResolver resolver;
    Knowledge knowledge;
    BlockingInteractorInterface iface;

    public Interpreter(KnowledgeResolver resolver,
                       Knowledge knowledge,
                       BlockingInteractorInterface iface, // TODO reduce number of parameters
                       PrepositionInterpreter prepositionInterpreter) {

        this.resolver = resolver;
        this.knowledge = knowledge;
        this.iface = iface;
        this.prepositionInterpreter = prepositionInterpreter;
        this.prepositionInterpreter.setInterpreter(this);
    }

    /**
     * Interpret a parse tree and produce a target.
     *
     * @param toInterpret An English sentence to interpret.
     * @param rootContext The context that the user can currently see.
     */
    List<EditorCommand> interpret(String toInterpret, CompositeModel rootContext) {

        List<EditorCommand> statements = new ArrayList<>();

        ParseTree tree = SyntaxNetLink.parse(toInterpret);

        SentencePart rootWord = tree.getRootWord();

        if (rootWord.getNature().startsWith("NN")) {

            interpretAsCreateCommand(statements, () -> rootContext, rootWord);

        } else if (InterpretPhrases.isCreationVerb(rootWord)) {

            SentencePart objectPhrase = rootWord.findFirstChild(phrase -> phrase.getRole().equals("dobj"));

            interpretAsCreateCommand(statements, () -> rootContext, objectPhrase);

            rootWord.findFirstChild(phrase -> phrase.getRole().equals("dobj"));

        } else if (rootWord.getNature().equals("VB") && rootWord.getRootWord().equalsIgnoreCase("is")) {

            processDefinition(rootWord);
        }

        return statements;
    }

    // Todo use command pattern here.
    private void processDefinition(SentencePart phrase) {
        SentencePart subj = phrase.dfsFind((SentencePart part) -> part.getRole().equals("nsub"));
        SentencePart obj = phrase.dfsFind((SentencePart part) -> part.getRole().equals("nobj"));

        if (obj == null) {
            throw new RuntimeException("Cannot find definition.");
        }

        Model currentSubjectDef = knowledge.getObject(subj.getRootWord());
        Model newSubjectDef = resolver.resolveObject(obj.getRootWord());

        if (currentSubjectDef == null) {
            if (iface.askUserYesNo("Do you want me to remember that " + subj.getRootWord() + " is " + newSubjectDef +
                    "?")) {
                knowledge.remember(subj.getRootWord(), newSubjectDef);
            }
        } else {

            String response = iface.askUserString("I currently know that " + subj.getRootWord() + " is " +
                    newSubjectDef + "."
                    + "Do you want be to replace the definition, add it as an alternative, or cancel? (replace / add " +
                    "/ cancel)");

            if (response.equalsIgnoreCase("replace")) {
                knowledge.remember(subj.getRootWord(), newSubjectDef);
            } else if (response.equalsIgnoreCase("add")) {
                if (currentSubjectDef instanceof AnyModel) {
                    ((AnyModel) currentSubjectDef).addOption(newSubjectDef);
                } else {
                    AnyModel any = new AnyModel(subj.getRootWord());
                    any.addOption(currentSubjectDef);
                    any.addOption(newSubjectDef);
                    knowledge.remember(subj.getRootWord(), any);
                }
            }
        }
    }


    /**
     * Generate a creation command for the object(s) described in this sentence part.
     *
     * @param statements A list of statements that come before interpreting this one.
     * @param scene A supplier that provides a scene when executed in which to create the object
     * @param obj The sentence part that we're trying to interpret
     */
    CreateEntityEditorCommand interpretAsCreateCommand(List<EditorCommand> statements,
                                                       Supplier<CompositeModel> scene,
                                                       SentencePart obj) {

        // Allocate a new command
        CreateEntityEditorCommand createStmt = new CreateEntityEditorCommand(scene);

        Stack<Model> modelStack = new Stack<>();

        // Check  if this is a known object
        createStmt.what = interpretModel(obj, modelStack);

        // Add the command at the end of the list so far
        statements.add(createStmt);

        return createStmt;
    }

    /**
     * Attempt to interpret the SentencePart as a model.
     * <p>
     * {@code sentence} must be rooted in something that can be interpreted as a Model.
     * <p>
     * The verb must already have been removed.
     * <p>
     * For example:
     * <p>
     * boats NNS dobj
     * +-- one CD num
     * |   +-- or CC cc
     * |       +-- two CD conj
     * <p>
     * But not:
     * <p>
     * Add VB ROOT
     * +-- boats NNS dobj
     * |   +-- one CD num
     * |       +-- or CC cc
     * |       +-- two CD conj
     *
     * @param phrase       What to interpret.
     * @param contextStack In which context to interpret it.
     * @return The Model that results.
     * <p>
     * Please note that the {@code contextStack} and its' contents may be modified
     * by interpretModel as sometimes deeper parts of the tree reveal things about
     * the context not known in advance. (Yay English!)
     */
    Model interpretModel(SentencePart phrase, Stack<Model> contextStack) {

        int stackSizeAtStart = contextStack.size();

        ///////////////////////////////
        // Resolve the object itself //
        ///////////////////////////////

        if (phrase.getNature().equals("NNS")) {
            String singular = InterpretPhrases.pluralToSingular(phrase.getRootWord());

            GroupModel model = new GroupModel(1, resolver.resolveObject(singular));

            SentencePart number = phrase.findFirstChild(c -> c.getNature().equals("CD"));

            model.number = InterpretPhrases.interpretInteger(number.getRootWord());

            contextStack.push(model);
        } else {
            contextStack.push(resolver.resolveObject(phrase.getRootWord()));
        }

        ////////////////////////////////////////////////////
        // TODO - Subordinate clauses to the main object. //
        ////////////////////////////////////////////////////

        // ---

        ///////////////////////
        // Process conjuncts //
        ///////////////////////

        SentencePart firstCC = phrase.findFirstChild(child -> child.getRole().equals("cc"));

        List<SentencePart> conjuncts = phrase.children.stream()
                .filter(child -> child.getRole().equals("conj"))
                .collect(Collectors.toList());


        if (firstCC != null) {
            if (firstCC.getRootWord().equalsIgnoreCase("and")) {

                CompositeModel compositeModel = new CompositeModel(phrase.getRootWord());

                Model topModel = contextStack.pop();

                compositeModel.addComponentForModel(topModel);

                contextStack.push(compositeModel);

                // Interpret the conjucts and add as components.
                for (SentencePart conjunct : conjuncts) {
                    compositeModel.addComponentForModel(interpretModel(conjunct, contextStack));
                }

            } else if (firstCC.getRootWord().equalsIgnoreCase("or")) {

                // Or-based conjuction, interpret as non-deterministic choice.
                AnyModel anyModel = new AnyModel(phrase.getRootWord());

                Model topModel = contextStack.pop();

                anyModel.addOption(topModel);

                contextStack.push(anyModel);

                // Interpret the conjuncts and add as options.
                for (SentencePart conjunct : conjuncts) {
                    anyModel.addOption(interpretModel(conjunct, contextStack));
                }

            } else {
                // This is an "or" or "while", don't know how to handle those yet!
                throw new RuntimeException("I don't know how to handle coordinating conjunction "
                        + firstCC.getRootWord() + " yet, sorry!");
            }
        }

        // Find any prepositions.
        // TODO distinguish between prepositions on the composite and on individual objects.
        for (SentencePart child : phrase.children) {
            if (child.getRole().equals("prep")) {
                prepositionInterpreter.processPreposition(child, contextStack);
            }
        }

        Model result = contextStack.pop();

        // Should not have changed.
        assert stackSizeAtStart == contextStack.size();

        return result;
    }

    /**
     * Explore a preposition.
     *
     * Ex: "above the field", "in the room", "through the tunnel"
     *
     * @param preposition The preposition
     * @param relatesTo A stack of models that the preposition might affect.
     *
     * {@code relatesTo} is a stack of models on which the preposition might be applied.
     *
     * For example, the phrase "a car on a road" will be broken down into "a car" and "on a road".
     * "a car" is evaluated first, then processPreposition is called with a stack with the {@link Model}
     * representing the car at the top.
     *
     * processPreposition would then pop the car Model off the stack, add a {@link CompositeModel} onto it,
     * create a road Model, add it as a component to the CompositeModel, add the car as a component to the
     * CompositeModel, and add a constraint to the CompositeModel that the car is on the road.
     */
    private void processPreposition(SentencePart preposition,
                                    Stack<Model> relatesTo) {

        // Allocate the constraint

        // Match the preposition against one of the known preposition patterns.

        /////////////////////////////////
        // Find the preposition object //
        /////////////////////////////////

        // Find what the preposition relates to.
        // For example, in "on a sphere", this would be "a sphere"

        // This is assumed to be impossible, but better check to make sure.

        // Find the determinant of the pobj

        /////////////////////////////
        // Find adverbial modifier //
        /////////////////////////////

        // Apply the constraint.
        prepositionInterpreter.processPreposition(preposition, relatesTo);
    }

}
