package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Interface.BlockingInteractor
import nl.wernerkroneman.Drawy.Modelling.Model

/**
 * Resolver that starts a description session.
 */
class RecursiveSessionResolver(private val interactor: BlockingInteractor) : ModelResolver {
    internal var sessionContext: DescriptionSession? = null

    fun setSessionContext(sessionContext: DescriptionSession) {
        this.sessionContext = sessionContext
    }

    /**
     * Resolve an object by starting a recursive session.
     *
     *
     * Use setSessionContext first.

     * @param name Name of the object.
     * *
     * @return The object
     * *
     * @pre sessionContext != null
     */
    override fun resolveObject(name: String): Model {

        if (sessionContext == null) {
            throw IllegalStateException("Session context not initialized.")
        }

        if (interactor.askUserYesNo("I don't know what '$name' is. Can you tell me?")) {

            interactor.tellUser("Ok, please tell me about " + name)

            val model = sessionContext!!.runSession()

            return model

        } else {
            interactor.tellUser("Ok... I guess?")
            throw RuntimeException("Uncooperative user.")
        }
    }
}
