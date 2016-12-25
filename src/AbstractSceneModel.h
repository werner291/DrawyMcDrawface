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

public:
  
  Entity(const std::string& name) : name(name) {}
  
  const std::string& getName()
  {
    return name;
  }
};

/**
 * The AbstractSceneModel represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 * 
 * It describes the scene on a very high level, and deals
 * in constraints rather than realisations of those constraints.
 */
class AbstractSceneModel {

    int idGen = 0;

public:
    std::vector<std::shared_ptr<Entity> > entities;
    //std::vector<constraint> entities;

    AbstractSceneModel();
    
    EntityPtr createEntity(const std::string& baseEntityName, std::string name = "");

    friend std::ostream &operator<<(std::ostream &stream, const AbstractSceneModel &scene);
};


#endif //DRAWYMCDRAWFACE_SCENECONTAINER_H
