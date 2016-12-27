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
public class Resolver {

    private TerminalInteractor interactor;
    private Knowledge knowledge;


    Resolver(TerminalInteractor interactor, Knowledge knowledge) {
        this.interactor = interactor;
        this.knowledge = knowledge;
    }

    Model resolveObject(String name) {

        Model toCreate = knowledge.getObject(name);

        if (toCreate == null) {
            askUserForObject(name);
        }

        return toCreate;
    }

    void askUserForObject(String name) {
        if (interactor.askUserYesNo("I don't know what '" + name + "' is. Can you tell me?")) {

            interactor.tellUser("Ok, please tell me about " + name);

            DescriptionSession session = new DescriptionSession();

            Model model = session.descriptionSession(name, knowledge);

            if (interactor.askUserYesNo("Would you like me to remember this?")) {
                knowledge.remember(name, model);
            } else {
                interactor.tellUser("Ok, I'll only use this description here. ");
            }

            interactor.tellUser("TIL what " + name + " looks like.");
        } else {
            interactor.tellUser("Ok... I guess?");
            throw new RuntimeException("Uncooperative user.");
        }
    }

}


//
// Created by werner on 26-12-16.
//


