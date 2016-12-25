/*
 * main.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

#include <thread>
#include <memory>

#include "ParseTree.h"
#include "AbstractSceneModel.h"
#include "Interpreter.h"
#include "Renderer.h"
#include "askUser.h"

using namespace std;

void showScene(std::shared_ptr<AbstractSceneModel> scene);

int main(int argc, char** argv) {

    shared_ptr<AbstractSceneModel> scene = make_shared<AbstractSceneModel>();
    Knowledge knowl;

    string line;

    while (getline(cin, line)) {
        auto results = interpret(line, scene, knowl, true);

        cout << "Interpreted: " << endl;

        for (auto stmt: results) {
            cout << stmt->describe() << endl;
        }

        if (results.empty()) {
            cout << "Unable to interpret. Guess I'm too stupid." << endl;
        }

        if (askUserYesNo("Is this correct?")) {
            for (auto cmd : results) {
                cmd->apply();
            }
        }

        showScene(scene);
    }
    
    //Renderer renderer(sceneComputer);
    
    //renderer.startRendering();
    
    ////////////////////
    // Shut down Ogre //
    ////////////////////

    return 0;
}

void showScene(std::shared_ptr<AbstractSceneModel> scene) {
    cout << "The scene currently looks like: " << endl;

    cout << *scene << endl;
}
