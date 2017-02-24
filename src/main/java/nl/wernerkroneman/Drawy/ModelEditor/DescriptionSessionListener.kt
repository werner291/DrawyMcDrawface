package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Modelling.Model

interface DescriptionSessionListener {

    /**
     * Called when the target in the description session has been changed.
     *
     *
     * Throughout the session, this may be the same object, but don't assume it.

     * @param scene
     */
    fun modelChanged(scene: Model)

    /**
     * Called when the sessionis ended and the target will no longer change.
     */
    fun sessionEnded()

    /**
     * Called when a session is started within a session.

     * @param recursiveSession
     */
    fun recursiveSessionStarted(recursiveSession: DescriptionSession)
}
