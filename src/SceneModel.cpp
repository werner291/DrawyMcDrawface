//
// Created by werner on 11-6-16.
//

#include "SceneModel.h"

SceneModel::SceneModel()
{
  entities.insert(std::make_pair(
      "primitive", 
      std::shared_ptr<Entity>(new Entity("primitive"))
    ));
}

EntityPtr SceneModel::createEntity(const std::string& baseEntityName, std::string name) {

    auto itr = entities.find(baseEntityName);
    if (itr == entities.end())
    {
        // TODO throw exception instead of this nonsense
        std::cout << "Cannot find base entity " << baseEntityName << std::endl;
        return std::shared_ptr<Entity>(nullptr);
    }

    if (name.empty())
    {
        name = std::to_string(idGen++);
    }
    else if (entities.find(name) != entities.end())
    {
        std::cout << "Duplicate entity " << name << std::endl;
        return std::shared_ptr<Entity>(nullptr);
    }

    std::shared_ptr<Entity> baseEntity = itr->second;

    std::shared_ptr<Entity> newEntity(new Entity(name));
    
    entities.insert(std::make_pair(name,newEntity));

    newEntity->addSuperEntity(baseEntity);
    
    std::cout << "Created entity: " << name << std::endl;
    
    return newEntity;
}

void SceneModel::addEntityToGroup(const std::string& entityName, 
				  const std::string& groupName)
{
  auto entItr = entities.find(entityName);
  if (entItr == entities.end())
  {
    std::cerr << "Cannot find entity named " << entityName << std::endl;
  }
  
  auto groupItr = entities.find(groupName);
  if (groupItr == entities.end())
  {
    std::cerr << "Cannot find entity group named " << groupName << std::endl;
  }
  
  groupItr->second->addSubEntity(entItr->second);
  
}

WeakEntityPtr SceneModel::getEntity(const std::string& entityName)
{
  auto entItr = entities.find(entityName);
  if (entItr == entities.end())
  {
    std::cerr << "Cannot find entity named " << entityName << std::endl;
    return EntityPtr(nullptr);
  }
  return entItr->second;
}
