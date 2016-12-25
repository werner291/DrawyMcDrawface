/*
 * main.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

#include "ParseTree.h"
#include "Interpreter.h"
#include "SceneModel.h"
#include "Renderer.h"

#include <thread>
#include <chrono>

int main(int argc, char** argv) {
    
    SceneModel scene;
    Knowledge knowl;

    std::string line;

    while (std::getline(std::cin, line)) {
        auto results = interpret(line, scene, knowl, true);

        std::cout << "Interpreted: " << std::endl;

        for (auto stmt: results) {
            std::cout << stmt->describe() << std::endl;
        }

        if (results.empty()) {
            std::cout << "Unable to interpret. Guess I'm too stupid." << std::endl;
        }
    }
    
    //Renderer renderer(sceneComputer);
    
    //renderer.startRendering();
    
    ////////////////////
    // Shut down Ogre //
    ////////////////////

    return 0;
}
