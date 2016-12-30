package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.Model;

public interface DescriptionSessionListener {

    /**
     * Called when the scene in the description session has been changed.
     * <p>
     * Throughout the session, this may be the same object, but don't assume it.
     *
     * @param scene
     */
    void modelChanged(Model scene);

    /**
     * Called when the sessionis ended and the scene will no longer change.
     */
    void sessionEnded();

    /**
     * Called when a session is started within a session.
     *
     * @param recursiveSession
     */
    void recursiveSessionStarted(DescriptionSession recursiveSession);
}
