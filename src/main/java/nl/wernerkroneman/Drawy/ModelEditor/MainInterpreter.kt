/*
 * Copyright (c) 2017 Werner Kroneman
 *
 * This file is part of DrawyMcDrawface.
 *
 * DrawyMcDrawface is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DrawyMcDrawface is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DrawyMcDrawface.  If not, see <http://www.gnu.org/licenses/>.
 */

package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.ModelEditor.Commands.EditorCommand
import nl.wernerkroneman.Drawy.Modelling.CompositeModel
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter

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
class MainInterpreter(val interpreter: PatternInterpreter = createDefaultModelInterpreter()) {

    class NoInterpretationException : RuntimeException()

    /**
     * Interpret a parse tree and produce a target.

     * @param toInterpret An English sentence to interpret.
     * *
     * @param rootContext The context that the user can currently see.
     */
    fun interpret(toInterpret: String,
                  rootContext: CompositeModel): EditorCommand {

        val tree = SyntaxNetLink.parse(toInterpret)

        val result = interpreter.interpret(tree,
                Any::class,
                context = emptyList())

        println("Interpreted as " + result)

        if (result !is EditorCommand) {
            throw RuntimeException("Top-level interpretation is not a command!")
        }

        return result
    }

}

