package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Interface.BlockingInteractor;
import nl.wernerkroneman.Drawy.Modelling.Model;

/**
 * Resolver that starts a description session.
 */
public class RecursiveSessionResolver implements ModelResolver {

    private final BlockingInteractor interactor;
    DescriptionSession sessionContext;

    public RecursiveSessionResolver(BlockingInteractor interactor) {
        this.interactor = interactor;
    }

    public void setSessionContext(DescriptionSession sessionContext) {
        this.sessionContext = sessionContext;
    }

    /**
     * Resolve an object by starting a recursive session.
     * <p>
     * Use setSessionContext first.
     *
     * @param name Name of the object.
     * @return The object
     * @pre sessionContext != null
     */
    @Override
    public Model resolveObject(String name) {

        if (sessionContext == null) {
            throw new IllegalStateException("Session context not initialized.");
        }

        if (interactor.askUserYesNo("I don't know what '" + name + "' is. Can you tell me?")) {

            interactor.tellUser("Ok, please tell me about " + name);

            Model model = sessionContext.runSession();

            return model;

        } else {
            interactor.tellUser("Ok... I guess?");
            throw new RuntimeException("Uncooperative user.");
        }
    }
}
