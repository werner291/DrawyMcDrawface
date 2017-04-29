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

import nl.wernerkroneman.Drawy.Interface.BlockingInteractor
import nl.wernerkroneman.Drawy.ModelEditor.Commands.EditorCommand
import nl.wernerkroneman.Drawy.Modelling.CompositeModelSpecification
import nl.wernerkroneman.Drawy.Modelling.CompositeModelSpecificationBase
import nl.wernerkroneman.Drawy.Modelling.ModelSpecification
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InterpretationContext
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter

/**
 * A class representing a session where a user describes an object.
 *
 *
 * It allows for asynchronous running of the session since other
 * parts of the program may need to run while this process takes its' time.
 */
class DescriptionSession(private val interpreter: PatternInterpreter = createDefaultModelInterpreter(),
                         private val interactorIface: BlockingInteractor) {

    private val changeListeners = mutableListOf<(ModelSpecification) -> Unit>()

    private val commandHistory = mutableListOf<EditorCommand>()

    fun start() {
        Thread(Runnable { this.runSession() }).start()
    }

    fun runSession(): ModelSpecification {
        val scene = CompositeModelSpecificationBase(name = "Scene")
        while (true) {

            val line = interactorIface.askUserString("Say something:")

            if (line == "done") {
                //notifyFinished()
                break
            }

            val result = interpreter.interpret<EditorCommand>(
                    SyntaxNetLink.parse(line),
                    context = listOf<InterpretationContext>(DescriptionSessionContext(commandHistory,
                            scene)))

            commandHistory.add(result)

            println("Interpreted as " + result)

            val stmt = result

//            interactorIface.tellUser("Interpreted: ")
//
//            interactorIface.tellUser(stmt.toString())
//
//            if (interactorIface.askUserYesNo("Is this correct?")) {
                stmt.apply()
//            }

            notifyChanged(scene)
        }
        return scene
    }

    private fun notifyChanged(scene: CompositeModelSpecification) {
        for (list in changeListeners.reversed()) {
            list(scene)
        }
    }

    fun addChangeListener(listener: (ModelSpecification) -> Unit) {
        changeListeners.add(listener)
    }

    /**
     * An interpretation context representing the description session
     * without exposing the session itself to interpretation.
     *
     * @param pastCommands A list of commands before the current interpretation.
     *                     No guarantees on execution state.
     *
     * @param scene The scene. Do not modify during interpretation.
     */
    class DescriptionSessionContext(val pastCommands: List<EditorCommand>,
                                    val scene: CompositeModelSpecification) : InterpretationContext
}