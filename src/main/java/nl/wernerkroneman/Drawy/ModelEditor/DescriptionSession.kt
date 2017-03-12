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
import nl.wernerkroneman.Drawy.Modelling.CompositeModel
import nl.wernerkroneman.Drawy.Modelling.Model
import java.util.*

/**
 * A class representing a session where a user describes an object.
 *
 *
 * It allows for asynchronous running of the session since other
 * parts of the program may need to run while this process takes its' time.
 */
class DescriptionSession(private val interpreter: MainInterpreter = MainInterpreter(),
                         private val interactorIface: BlockingInteractor) {

    private val changeListeners = mutableListOf<(Model) -> Unit>()

    fun start() {
        Thread(Runnable { this.runSession() }).start()
    }

    fun runSession(): Model {
        val scene = CompositeModel("Scene")
        while (true) {
            val line = interactorIface.askUserString("Say something:")

            if (line == "done") {
                //notifyFinished()
                break
            }

            val stmt = interpreter.interpret(line, scene)

            interactorIface.tellUser("Interpreted: ")

            interactorIface.tellUser(stmt.toString())

            if (interactorIface.askUserYesNo("Is this correct?")) {
                stmt.apply()
            }

            notifyChanged(scene)
        }
        return scene
    }

    private fun notifyChanged(scene: CompositeModel) {
        for (list in changeListeners.reversed()) {
            list(scene)
        }
    }

    fun addChangeListener(listener: (Model) -> Unit) {
        changeListeners.add(listener)
    }
}