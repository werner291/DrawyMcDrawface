package nl.wernerkroneman.Drawy.ModelEditor;


import nl.wernerkroneman.Drawy.Modelling.CompositeModel;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Supplier;

import static nl.wernerkroneman.Drawy.ModelEditor.RelativePositionStatement.RelativePosition.ABOVE;

public class Interpreter {

    KnowledgeResolver resolver;

    public Interpreter(KnowledgeResolver resolver) {
        this.resolver = resolver;
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

        assert tree != null;
        SentencePart rootWord = tree.getRootWord();

        if (!rootWord.getNature().equals("VB") || InterpretPhrases.isCreationVerb(rootWord)) {
            // This is a command in the form of "Create a cube", or simply "a cube"

            SentencePart obj = rootWord.dfsFind((SentencePart part) -> part.nature.startsWith("NN"));

            if (obj == null) {
                throw new RuntimeException("Cannot find object to create.");
            }

            creationRuleForObject(statements, () -> rootContext, obj);
        }

        return statements;
    }


    /**
     * Generate a creation command for the object(s) described in this sentence part.
     *
     * @param statements A list of statements that come before interpreting this one.
     * @param scene A supplier that provides a scene when executed in which to create the object
     * @param obj The sentence part that we're trying to interpret
     */
    CreateEntityEditorCommand creationRuleForObject(List<EditorCommand> statements,
                                                    Supplier<CompositeModel> scene,
                                                    SentencePart obj) {

        // Allocate a new command
        CreateEntityEditorCommand createStmt = new CreateEntityEditorCommand(scene);

        // Check whether plural and singular
        String objName;

        if (obj.getNature().equals("NNS")) {
            // Plural
            createStmt.number = InterpretPhrases.findNumber(obj);
            objName = InterpretPhrases.pluralToSingular(obj.getRootWord());
        } else {
            // Singular
            createStmt.number = 1;
            objName = obj.getRootWord();
        }

        // Check  if this is a known object
        createStmt.what = resolver.resolveObject(objName);

        // Add the command at the end of the list so far
        statements.add(createStmt);

        for (Iterator<SentencePart> itr = obj.children.iterator(); itr.hasNext();) {
            SentencePart part = itr.next();
            // Look for "and something else"-type phrases.
            if (part.getRole().equals("cc")) {
                if (! part.getRootWord().equals("and")) {
                    // This is an "or" or "while", don't know how to handle those yet!
                    throw new RuntimeException("I don't know how to handle coordinating conjunction "
                            + part.getRootWord() + " yet, sorry!");
                }

                // Add separate creation rules for the conjuncts as well.
                creationRuleForObject(statements, scene, itr.next());
            }

            if (part.getRole().equals("prep")) {
                processPreposition(part, createStmt, statements);
            }
        }

        return createStmt;
    }

    private void processPreposition(SentencePart preposition,
                                    CreateEntityEditorCommand relatesTo,
                                    List<EditorCommand> statements) {
        // Find what the preposition relates to.
        // For example, in "on a sphere", this would be "a sphere"
        SentencePart prepObj = preposition.dfsFind(part -> part.getRole().equals("pobj"));

        // Find the determinant of the pobj
        SentencePart pobjDet = prepObj.dfsFind(part -> part.getRole().equals("det"));

        RelativePositionStatement.RelativePosition relativePosition;

        if (preposition.getRootWord().equalsIgnoreCase("above")) {
            relativePosition = ABOVE;
        } else {
            throw new UnsupportedOperationException("I don't know the preposition " + preposition.getRootWord());
        }

        if ((pobjDet == null && prepObj.getNature().startsWith("NN")) || pobjDet.getRootWord().equalsIgnoreCase("a")) {
            CreateEntityEditorCommand result = creationRuleForObject(statements, relatesTo.target, prepObj);

            statements.add(new RelativePositionStatement(relatesTo.getResultSupplier(), result.getResultSupplier(),
                    relativePosition, relatesTo.target));
        } else if (relatesTo.number >= 2 && InterpretPhrases.isReciprocalPronoun(preposition)) {
            //statements.add(new RelativePositionStatement(relatesTo, null, relativePosition, relatesTo
            // .getResultProvider()));
        } else {
            throw new UnsupportedOperationException("Selectors not yet implemented.");
        }


    }

}
