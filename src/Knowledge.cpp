//
// Created by werner on 25-12-16.
//

#include "Knowledge.h"

#include "CompositeModel.h"

void initializeKnowledgeWithPrimitives(Knowledge &knowledge) {
    knowledge.remember("Cube", std::make_shared<PrimitiveModel>(ShapeType::CUBE, "Cube"));
    knowledge.remember("Cylinder", std::make_shared<PrimitiveModel>(ShapeType::CYLINDER, "Cylinder"));
    knowledge.remember("Sphere", std::make_shared<PrimitiveModel>(ShapeType::SPHERE, "Sphere"));
}