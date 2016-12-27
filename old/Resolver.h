//
// Created by werner on 26-12-16.
//

#ifndef DRAWYMCDRAWFACE_RESOLVER_H
#define DRAWYMCDRAWFACE_RESOLVER_H

#include "Knowledge.h"
#include "workflow.h"
#include "askUser.h"

/**
 * Class that assists the Interpreter in finding objects
 * that the user is referring to. If necessary, it asks
 * questions to the user through the UserInteractor.
 *
 * This class explicitly does not modify existing objects, although
 * they may be modified during interaction with the user.
 */
template<typename Clarifier = UserInteractor>
class Resolver {

private:
    Knowledge &knowledge;

public:
    Resolver(Clarifier &interactor, Knowledge &knowledge) : interactor(interactor), knowledge(knowledge) { }

    Model> resolveObject(const String &name) {

        Model> toCreate = knowledge.getObject(name);

        if (toCreate.get() == nullptr) {
            if (Clarifier::askUserYesNo("I don't know what '" + name + "' is. Can you tell me?")) {

                interactor.tellUser("Ok, please tell me about " + name);

                auto model = descriptionSession(name, knowledge);

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

        return toCreate;
    }

};


#endif //DRAWYMCDRAWFACE_RESOLVER_H
