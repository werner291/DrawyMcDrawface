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
#include "SceneContainer.h"

class Interpreter
{

    SceneContainer& sceneContainer;

public:
    Interpreter(SceneContainer& sceneContainer);
    virtual ~Interpreter();

    bool interpretParsed(const ParseTree& parsetree);

    /**
     * Interpret the given text as an integer.
     *
     * \returns A pair containing an int (the result) and a
     * 	    float (confidence, between 0 and 1).
     * 	    If the interpretation failed, the confidence will be exactly 0.
     */
    std::pair<int, float> interpretInteger(const std::string& text);
protected:
    std::pair<std::string, bool> pluralToSingular(const std::string& name);
};

#endif /* MANIPULATOR_H_ */
