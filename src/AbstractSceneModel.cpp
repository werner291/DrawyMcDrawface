//
// Created by werner on 11-6-16.
//

#include "AbstractSceneModel.h"

AbstractSceneModel::AbstractSceneModel()
{
}

EntityPtr AbstractSceneModel::createEntity(const std::string &baseEntityName, std::string name) {

    if (name.empty())
    {
        name = std::to_string(idGen++);
    }

    std::shared_ptr<Entity> newEntity(new Entity(name));

    entities.push_back(newEntity);
    
    return newEntity;
}

std::ostream &operator<<(std::ostream &stream, const AbstractSceneModel &scene) {
    stream << "Entities:" << std::endl;
    for (auto ent : scene.entities) {
        stream << " - " << ent->getName() << std::endl;
    }
}