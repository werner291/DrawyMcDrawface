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

int main(int argc, char** argv)
{
    ParseTree tree(
        R"(Let VB ROOT
 +-- be VB ccomp
     +-- there EX expl
     +-- cubes NNS nsubj
         +-- 15 CD num)");

    std::cout << "----------------" << std::endl;
    
    SceneModel scene;
    
    SceneComputer sceneComputer(scene);

    Interpreter interp(scene);

    interp.interpretParsed(tree);
    
    Renderer renderer(sceneComputer);
    
    renderer.startRendering();
    
    ////////////////////
    // Shut down Ogre //
    ////////////////////

    

    return 0;
}
