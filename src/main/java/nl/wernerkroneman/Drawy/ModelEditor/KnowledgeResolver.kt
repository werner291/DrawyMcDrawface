package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Interface.BlockingInteractor
import nl.wernerkroneman.Drawy.Modelling.Model

import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.IOException
import java.io.ObjectOutputStream

/**
 * Class that assists the Interpreter in finding objects
 * that the user is referring to. If necessary, it asks
 * questions to the user through the UserInteractor.

 * This class explicitly does not modify existing objects, although
 * they may be modified during interaction with the user.
 */
class KnowledgeResolver(private val knowledge: Knowledge,
                        private val interactor: BlockingInteractor,
                        internal var fallback: ModelResolver?) : ModelResolver {

    fun setFallback(fallback: ModelResolver) {
        this.fallback = fallback
    }

    override fun resolveObject(name: String): Model {

        var toCreate: Model? = knowledge.getObject(name)

        if (toCreate == null) {

            if (fallback == null) {
                throw IllegalStateException("No fallback resolver")
            }

            toCreate = fallback!!.resolveObject(name)

            interactor.tellUser("TIL what $name looks like.")

            if (interactor.askUserYesNo("Would you like me to remember this?")) {
                knowledge.remember(toCreate)

                try {
                    var fout: FileOutputStream? = null
                    fout = FileOutputStream("knowledge.txt")

                    val out = ObjectOutputStream(fout)

                    out.writeObject(knowledge)
                    out.flush()
                } catch (e: FileNotFoundException) {
                    e.printStackTrace()
                } catch (e: IOException) {
                    e.printStackTrace()
                }

            } else {
                interactor.tellUser("Ok, I'll only use this description here. ")
            }
        }

        return toCreate
    }
}


//
// Created by werner on 26-12-16.
//


