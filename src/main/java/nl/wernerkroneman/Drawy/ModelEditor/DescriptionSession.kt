package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Interface.BlockingInteractor
import nl.wernerkroneman.Drawy.Modelling.CompositeModel
import nl.wernerkroneman.Drawy.Modelling.Model

import java.util.ArrayList

/**
 * A class representing a session where a user describes an object.
 *
 *
 * It allows for asynchronous running of the session since other
 * parts of the program may need to run while this process takes its' time.
 */
class DescriptionSession(private val interpreter: MainInterpreter, private val interactorIface: BlockingInteractor) {
    private val listeners = ArrayList<DescriptionSessionListener>()

    fun start() {
        Thread(Runnable { this.runSession() }).start()
    }

    fun runSession(): Model {
        val scene = CompositeModel("Scene")
        while (true) {
            val line = interactorIface.askUserString("Say something:")

            if (line == "done") {
                notifyFinished()
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

    private fun notifyFinished() {
        for (list in listeners) {
            list.sessionEnded()
        }
    }

    fun addListener(listener: DescriptionSessionListener) {
        listeners.add(listener)
    }

    private fun notifyChanged(scene: CompositeModel) {
        for (list in listeners) {
            list.modelChanged(scene)
        }
    }
}