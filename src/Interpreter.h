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
#include "AbstractSceneModel.h"
#include "SceneCommand.h"
#include "Knowledge.h"

/**
 * Interpret a parse tree and produce a scene.
 *
 * @param parseTree The parsed text
 * @param previous What the scene looked like before processing this text.
 */
std::vector<std::shared_ptr<SceneCommand> > interpret(const std::string &toInterpret,
                                                      std::shared_ptr<AbstractSceneModel> scene,
                                                      Knowledge &knowledge,
                                                      bool allowLearning = true);


#endif /* MANIPULATOR_H_ */
