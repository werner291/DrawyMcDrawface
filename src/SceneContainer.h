//
// Created by werner on 11-6-16.
//

#ifndef DRAWYMCDRAWFACE_SCENECONTAINER_H
#define DRAWYMCDRAWFACE_SCENECONTAINER_H

#include <boost/container/flat_set.hpp>
#include <OGRE/OgreVector3.h>

class Entity;

typedef std::weak_ptr<Entity> WeakEntityPtr;

struct weak_ptr_comparator{
  bool operator()(const WeakEntityPtr& a, const WeakEntityPtr& b) const
  {
    return a.lock() < b.lock();
  }
};

typedef boost::container::flat_set< WeakEntityPtr, weak_ptr_comparator > EntityWeakPointerSet;

class Entity {
public:
  std::string name;
  EntityWeakPointerSet superEntities;
  EntityWeakPointerSet subEntities;
  Ogre::Vector3 position;
  
  Entity(const std::string& name) : name(name) {}
  
  void addSuperEntity(WeakEntityPtr superEnt)
  {
    superEntities.insert(superEnt);
  }
  
  void addSubEntity(WeakEntityPtr subEnt)
  {
    subEntities.insert(subEnt);
  }
};

/**
 * The SceneContainer represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 * 
 * It describes the scene on a very high level, and deals
 * in constraints than realisations of those constraints.
 */
class SceneContainer {
  
  int idGen;

public:
    std::map<std::string, std::shared_ptr<Entity> > entities;
    
    bool createEntity(const std::string& baseEntityName, std::string name = "");
};


#endif //DRAWYMCDRAWFACE_SCENECONTAINER_H
