/*
 * Manipulator.h
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

#ifndef MANIPULATOR_H_
#define MANIPULATOR_H_

#include <string>
#include "ParseTree.h"
#include "CompositeModel.h"
#include "SceneCommand.h"
#include "Knowledge.h"
#include "Resolver.h"

class Interpreter {

    Resolver &resolver;

public:
    Interpreter(Resolver &resolver) : resolver(resolver) { }

    /**
     * Interpret a parse tree and produce a scene.
     *
     * @param parseTree The parsed text
     * @param previous What the scene looked like before processing this text.
     */
    std::vector<SceneCommand> > interpret(const String &toInterpret,
                                                          CompositeModel> scene);
}

#endif /* MANIPULATOR_H_ */
