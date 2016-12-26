/*
 * main.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

#include <thread>
#include <memory>

#include "workflow.h"

using namespace std;

int main(int argc, char** argv) {

    Knowledge knowledge;

    initializeKnowledgeWithPrimitives(knowledge);

    descriptionSession("Main scene", knowledge);
    
    ////////////////////
    // Shut down Ogre //
    ////////////////////

    return 0;
}

