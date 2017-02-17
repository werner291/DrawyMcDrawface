package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.MainMatcher;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhrasePattern;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTreeNodeBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.function.Function;
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
public class Interpreter extends MainMatcher {

    KnowledgeResolver resolver;
    Knowledge knowledge;
    BlockingInteractorInterface iface;

    public Interpreter(KnowledgeResolver resolver,
                       Knowledge knowledge,
                       BlockingInteractorInterface iface) {

        this.resolver = resolver;
        this.knowledge = knowledge;
        this.iface = iface;

        this.addPattern(new PhrasePattern(CreateEntityEditorCommand.class,
                new PhraseTreeNodeBuilder()
                        .setNature("NN")
                        .setName("what")
                        .createPhraseTreeNode(),
                createCommandProcessor));

        this.addPattern(new PhrasePattern(CreateEntityEditorCommand.class,
                new PhraseTreeNodeBuilder()
                        .setWord("Give")
                        .addChild(new PhraseTreeNodeBuilder()
                                        .setWord("me")
                                        .createPhraseTreeNode())
                        .addChild(new PhraseTreeNodeBuilder()
                                        .setRequiredSubpatternType(Model.class)
                                        .createPhraseTreeNode())
                        .addChild(new PhraseTreeNodeBuilder()
                                        .setRole("punct")
                                        .createPhraseTreeNode()
                ).createPhraseTreeNode(),
                createCommandProcessor));

        this.addPattern(new PhrasePattern(CreateEntityEditorCommand.class,
                new PhraseTreeNodeBuilder()
                        .setWord("Create")
                        .addChild(new PhraseTreeNodeBuilder()
                                        .setWord("me")
                                        .createPhraseTreeNode())
                        .addChild(new PhraseTreeNodeBuilder()
                                        .setRequiredSubpatternType(Model.class)
                                        .createPhraseTreeNode())
                        .addChild(new PhraseTreeNodeBuilder()
                                        .setRole("punct")
                                        .createPhraseTreeNode()
                ).createPhraseTreeNode(),
                createCommandProcessor));
    }

    /**
     * Interpret a parse tree and produce a target.
     *
     * @param toInterpret An English sentence to interpret.
     * @param rootContext The context that the user can currently see.
     */
    List<EditorCommand> interpret(String toInterpret, CompositeModel rootContext) {

        List<EditorCommand> statements = new ArrayList<>();

        PhraseTree tree = SyntaxNetLink.parse(toInterpret);

        Object result = match(tree);

        System.out.println("Interpreted as " + result.toString());

        return statements;
    }

    /**
     * Generate a creation command for the object(s) described in this sentence part.
     *
     * @param statements A list of statements that come before interpreting this one.
     * @param scene A supplier that provides a scene when executed in which to create the object
     */
    Function<PhrasePattern.MatchResult, Object> createCommandProcessor =
            (PhrasePattern.MatchResult result) -> {

        // Allocate a new command, TODO supply the scene somehow
        CreateEntityEditorCommand createStmt = new CreateEntityEditorCommand(null);

        Stack<Model> modelStack = new Stack<>();

        createStmt.what = (Model) result.dependencies.get("what");

        return createStmt;
    };

    Function<PhrasePattern.MatchResult, Object> modelProcessor =
            (PhrasePattern.MatchResult result) -> {

        return null;
    };

}
