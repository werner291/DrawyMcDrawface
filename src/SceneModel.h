//
// Created by werner on 11-6-16.
//

#ifndef DRAWYMCDRAWFACE_SCENECONTAINER_H
#define DRAWYMCDRAWFACE_SCENECONTAINER_H

#include <boost/container/flat_set.hpp>
#include <OGRE/OgreVector3.h>

class Entity;

typedef std::weak_ptr<Entity> WeakEntityPtr;
typedef std::shared_ptr<Entity> EntityPtr;

struct weak_ptr_comparator{
  bool operator()(const WeakEntityPtr& a, const WeakEntityPtr& b) const
  {
    return a.lock() < b.lock();
  }
};

typedef boost::container::flat_set< WeakEntityPtr, weak_ptr_comparator > EntityWeakPointerSet;

class Entity : public std::enable_shared_from_this<Entity>{
  
  std::string name;
  EntityWeakPointerSet superEntities;
  EntityWeakPointerSet subEntities;
  
public:
  
  Entity(const std::string& name) : name(name) {}
  
  const EntityWeakPointerSet& getSubEntities() const
  {
    return subEntities;
  }
  
  const EntityWeakPointerSet& getSuperEntities() const
  {
    return superEntities;
  }
  
  const WeakEntityPtr findAncestor(const std::string& name)
  {
    for (WeakEntityPtr parent : superEntities)
    {
      if (parent.lock()->name == name)
	return parent;
      
      WeakEntityPtr parentResult = parent.lock()->findAncestor(name);
      
      if (parentResult.lock().get() != nullptr)
      {
	return parentResult;
      }
    }
    
    return EntityPtr(nullptr);
  }
  
  void addSuperEntity(WeakEntityPtr superEnt)
  {
    auto result = superEntities.insert(superEnt);	
    if (result.second)
    {
      superEnt.lock()->addSubEntity(shared_from_this());
    }
  }
  
  void addSubEntity(WeakEntityPtr subEnt)
  {
    auto result = subEntities.insert(subEnt);
    if (result.second)
    {
      subEnt.lock()->addSuperEntity(shared_from_this());
    }
  }
  
  const std::string& getName()
  {
    return name;
  }
};

/**
 * The SceneModel represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 * 
 * It describes the scene on a very high level, and deals
 * in constraints rather than realisations of those constraints.
 */
class SceneModel {
  
  int idGen;

public:
    std::map<std::string, std::shared_ptr<Entity> > entities;
    
    SceneModel();
    
    EntityPtr createEntity(const std::string& baseEntityName, std::string name = "");
    
    void addEntityToGroup(const std::string& entityName, const std::string& groupName);
    WeakEntityPtr getEntity(const std::string& toDraw);
};


#endif //DRAWYMCDRAWFACE_SCENECONTAINER_H
