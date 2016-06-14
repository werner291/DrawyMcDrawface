//
// Created by werner on 11-6-16.
//

#include "SceneContainer.h"

bool SceneContainer::createEntity(const std::string& baseEntityName, std::string name) {

    auto itr = entities.find(baseEntityName);
    if (itr == entities.end())
    {
        // TODO throw exception instead of this nonsense
        std::cout << "Cannot find base entity " << baseEntityName << std::endl;
        return false;
    }

    if (name.empty())
    {
        name = std::to_string(idGen++);
    }
    else if (entities.find(name) != entities.end())
    {
        std::cout << "Duplicate entity " << name << std::endl;
        return false;
    }

    std::shared_ptr<Entity> baseEntity = itr->second;

    std::shared_ptr<Entity> newEntity(new Entity(name));

    baseEntity->addSubEntity(newEntity);
    newEntity->addSuperEntity(baseEntity);

    newEntity->position.x = 0;
    newEntity->position.y = 0;
    newEntity->position.z = 0;
}
