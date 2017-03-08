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

package nl.wernerkroneman.Drawy.Interface

import nl.wernerkroneman.Drawy.ModelEditor.BlockingInteractorInterface
import java.util.concurrent.CountDownLatch
import javax.swing.SwingUtilities

/**
 * An interface to the UserInteractor that allows
 * calling it from another thread and blocking
 * until a response is given.
 */
class BlockingInteractor(internal var interactor: UserInteractor) : BlockingInteractorInterface {

    override fun askUserString(question: String): String {

        assert(!SwingUtilities.isEventDispatchThread())

        val latch = CountDownLatch(1)

        class StringWrapper {
            var str: String? = null
        }

        val response = StringWrapper()

        SwingUtilities.invokeLater {
            interactor.putText(question)

            interactor.addListener { text ->
                response.str = text
                latch.countDown()
                false
            }
        }

        while (response.str == null) {
            try {
                latch.await()
            } catch (e: InterruptedException) {
                e.printStackTrace()
            }

        }

        return response.str as String

    }

    override fun askUserYesNo(question: String): Boolean {

        while (true) {

            val line = askUserString(question + "(Y/N)")

            if (line.equals("Y", ignoreCase = true) || line.equals("Yes", ignoreCase = true)) {
                return true
            } else if (line.equals("N", ignoreCase = true) || line.equals("No", ignoreCase = true)) {
                return false
            }

            println("Invalid entry: " + line)
        }

    }

    override fun tellUser(toTell: String) {
        SwingUtilities.invokeLater { interactor.putText(toTell) }
    }
}