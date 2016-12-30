package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.Model;

/**
 * Class that assists the Interpreter in finding objects
 * that the user is referring to. If necessary, it asks
 * questions to the user through the UserInteractor.
 *
 * This class explicitly does not modify existing objects, although
 * they may be modified during interaction with the user.
 */
public class KnowledgeResolver implements ModelResolver {

    private final BlockingInteractorInterface interactor;
    ModelResolver fallback;
    private Knowledge knowledge;

    public KnowledgeResolver(Knowledge knowledge, BlockingInteractorInterface interactor, ModelResolver fallback) {
        this.knowledge = knowledge;
        this.interactor = interactor;
        this.fallback = fallback;
    }

    public void setFallback(ModelResolver fallback) {
        this.fallback = fallback;
    }

    @Override
    public Model resolveObject(String name) {

        Model toCreate = knowledge.getObject(name);

        if (toCreate == null) {

            if (fallback == null) {
                throw new IllegalStateException("No fallback resolver");
            }

            toCreate = fallback.resolveObject(name);

            interactor.tellUser("TIL what " + name + " looks like.");

            if (interactor.askUserYesNo("Would you like me to remember this?")) {
                knowledge.remember(name, toCreate);
            } else {
                interactor.tellUser("Ok, I'll only use this description here. ");
            }
        }

        return toCreate;
    }
}


//
// Created by werner on 26-12-16.
//


