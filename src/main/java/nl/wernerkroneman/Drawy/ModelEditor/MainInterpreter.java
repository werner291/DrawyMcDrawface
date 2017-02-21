package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhrasePattern;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhrasePatternBuilder;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * The class that houses the most important, top-level parts
 * of the system that turns parsed English sentences commands
 * that the system can work with.
 * <p>
 * If the system sometimes looks a bit spaghetti-ish, please remember
 * that the language we're trying to understand is not actually meant
 * to be a good, simple and unambiguous language. People have probably
 * literally been beheaded in the process.
 */
public class MainInterpreter {

    Collection<InterpreterEntry> patterns = new ArrayList<>();

    public MainInterpreter() {
        
        CreateCommandInterpreter createCommandInterpreter =
                new CreateCommandInterpreter(this);

        patterns.add(new InterpreterEntry(createCommandInterpreter,
                new PhrasePatternBuilder()
                        .setRole("ROOT")
                        .setName("what")
                        .create()));

        patterns.add(new InterpreterEntry(createCommandInterpreter,
                new PhrasePatternBuilder()
                        .setWord("Give")
                        .addChild(new PhrasePatternBuilder()
                                        .setWord("me")
                                        .create())
                        .addChild(new PhrasePatternBuilder()
                                        .setName("what")
                                        .create())
                        .addChild(new PhrasePatternBuilder()
                                        .setRole("punct")
                                        .create()
                ).create()));

        patterns.add(new InterpreterEntry(createCommandInterpreter,
                new PhrasePatternBuilder()
                        .setWord("Create")
                        .addChild(new PhrasePatternBuilder()
                                        .setName("what")
                                        .create())
                        .addChild(new PhrasePatternBuilder()
                                        .setRole("punct")
                                        .create()
                ).create()));
    }

    /**
     * Interpret a parse tree and produce a target.
     *
     * @param toInterpret An English sentence to interpret.
     * @param rootContext The context that the user can currently see.
     */
    EditorCommand interpret(String toInterpret, CompositeModel rootContext) {

        List<EditorCommand> statements = new ArrayList<>();

        PhraseTree tree = SyntaxNetLink.parse(toInterpret);

        Object result = this.interpret(tree, (InterpreterEntry e) -> {
            return EditorCommand.class.isAssignableFrom(
                            e.objectFactory.getInterpretedTypePrediction());
        });

        System.out.println("Interpreted as " + result);

        return null;
    }

    Function<PhrasePattern.MatchResult, Object> modelProcessor =
            (PhrasePattern.MatchResult result) -> {

        return null;
    };

    /**
     * Attempt to interpret the phrase according to the interpretation rules.
     * @param phrase
     * @param filter
     * @return
     */
    Object interpret(PhraseTree phrase, Predicate<InterpreterEntry> filter) {
        for (InterpreterEntry entry: patterns) {
            if (filter.test(entry)) {
                PhrasePattern.MatchResult result = entry.pattern.matchAgainst(phrase);

                if (result.matches) {
                    return entry.objectFactory.createObject(result.capturings);
                }
            }
        }
        return null;
    }

    /**
     * An interface for classes that take the capturings
     * from a {@link PhrasePattern.MatchResult} and returns
     * the interpretation of it.
     *
     * It may return null if the interpretation fails.
     *
     * @param <E> The type of object that results from interpretation.
     */
    public interface InterpretedObjectFactory<E> {

        Class getInterpretedTypePrediction();

        E createObject(Map<String, PhraseTree> capturings);
    }

    /**
     * Represents an interpretation rule.
     * It consists of a {@link PhrasePattern} that specifies
     * on which kind of phrases it will apply, and an
     * {@link InterpretedObjectFactory} that serves to get the
     * final object.
     */
    public static class InterpreterEntry {
        InterpretedObjectFactory objectFactory;
        PhrasePattern pattern;

        public InterpreterEntry(InterpretedObjectFactory objectFactory,
                                PhrasePattern pattern) {
            this.objectFactory = objectFactory;
            this.pattern = pattern;
        }
    }
}
