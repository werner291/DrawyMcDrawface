package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.ModelEditor.Interpreters.CreateCommandInterpreter
import nl.wernerkroneman.Drawy.ModelEditor.Interpreters.KnowledgeModelInterpreter
import nl.wernerkroneman.Drawy.Modelling.*
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhrasePattern
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhrasePatternBuilder

import java.util.*
import java.util.function.Predicate

/**
 * The class that houses the most important, top-level parts
 * of the system that turns parsed English sentences commands
 * that the system can work with.
 *
 *
 * If the system sometimes looks a bit spaghetti-ish, please remember
 * that the language we're trying to understand is not actually meant
 * to be a good, simple and unambiguous language. People have probably
 * literally been beheaded in the process.
 */
class MainInterpreter {

    internal var patterns: MutableCollection<InterpreterEntry> = ArrayList()

    init {

        val knowledge = Knowledge.knowledgeWithPrimitives()

        val createCommandInterpreter = CreateCommandInterpreter(this)

        val knowledgeModelInterpreter = KnowledgeModelInterpreter(this, knowledge)

        patterns.add(InterpreterEntry(createCommandInterpreter,
                PhrasePatternBuilder()
                        .setRole("ROOT")
                        .setNature("NN")
                        .setName("what")
                        .create()))

        patterns.add(InterpreterEntry(createCommandInterpreter,
                PhrasePatternBuilder()
                        .setWord("Give")
                        .addChild(PhrasePatternBuilder()
                                .setWord("me")
                                .create())
                        .addChild(PhrasePatternBuilder()
                                .setName("what")
                                .create())
                        .addChild(PhrasePatternBuilder()
                                .setRole("punct")
                                .create()
                        ).create()))

        patterns.add(InterpreterEntry(createCommandInterpreter,
                PhrasePatternBuilder()
                        .setWord("Create")
                        .addChild(PhrasePatternBuilder()
                                .setName("what")
                                .create())
                        .addChild(PhrasePatternBuilder()
                                .setRole("punct")
                                .create()
                        ).create()))

        patterns.add(InterpreterEntry(knowledgeModelInterpreter,
                PhrasePatternBuilder()
                        .setNature("NN")
                        .setName("object name")
                        .create()))
    }

    /**
     * Interpret a parse tree and produce a target.

     * @param toInterpret An English sentence to interpret.
     * *
     * @param rootContext The context that the user can currently see.
     */
    public fun interpret(toInterpret: String, rootContext: CompositeModel): EditorCommand {

        val statements = ArrayList<EditorCommand>()

        val tree = SyntaxNetLink.parse(toInterpret)

        val result = this.interpret(tree, {
            entry : InterpreterEntry -> EditorCommand::class.java.isAssignableFrom(
                entry.objectFactory.interpretedTypePrediction)
        })

        println("Interpreted as " + result!!)

        if (result !is EditorCommand) {
            throw RuntimeException(":(")
        }

        return result
    }

    /**
     * Attempt to interpret the phrase according to the interpretation rules.
     * @param phrase
     * *
     * @param filter
     * *
     * @return
     */
    internal fun interpret(phrase: PhraseTree, filter: (InterpreterEntry) -> Boolean): Any? {
        for (entry in patterns) {
            if (filter(entry)) {
                val result = entry.pattern.matchAgainst(phrase)

                if (result.matches) {
                    return entry.objectFactory.createObject(result.capturings)
                }
            }
        }
        return null
    }

    /**
     * An interface for classes that take the capturings
     * from a [PhrasePattern.MatchResult] and returns
     * the interpretation of it.

     * It may return null if the interpretation fails.

     * @param <E> The type of object that results from interpretation.
    </E> */
    interface InterpretedObjectFactory<E> {

        val interpretedTypePrediction: Class<*>

        fun createObject(capturings: Map<String, PhraseTree>): Any?
    }

    /**
     * Represents an interpretation rule.
     * It consists of a [PhrasePattern] that specifies
     * on which kind of phrases it will apply, and an
     * [InterpretedObjectFactory] that serves to get the
     * final object.
     */
    class InterpreterEntry(internal var objectFactory: InterpretedObjectFactory<*>,
                           internal var pattern: PhrasePattern)
}
