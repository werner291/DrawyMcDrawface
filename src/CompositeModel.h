//
// Created by werner on 11-6-16.
//

#ifndef DRAWYMCDRAWFACE_SCENECONTAINER_H
#define DRAWYMCDRAWFACE_SCENECONTAINER_H

#include <boost/container/flat_set.hpp>
#include <OGRE/OgreVector3.h>

class Model;

typedef std::weak_ptr<Model> WeakEntityPtr;
typedef std::shared_ptr<Model> EntityPtr;

class Model {
  
  std::string name;

public:

    Model(const std::string &name) : name(name) { }
  
  const std::string& getName()
  {
    return name;
  }
};

enum ShapeType {
    CUBE, SPHERE, CYLINDER
};

class PrimitiveModel : public Model {
    // A bit unelegant right now, will eventually replace with something mesh-based
    // where something class-based actually makes sense.

    ShapeType shape;

public:

    PrimitiveModel(ShapeType shape, const std::string &name) : Model(name), shape(shape) { }
};

/**
 * The CompositeModel represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 * 
 * It describes the scene on a very high level, and deals
 * in constraints rather than realisations of those constraints.
 */
class CompositeModel : public Model {

    int idGen = 0;

public:
    std::vector<std::shared_ptr<Model> > entities;
    //std::vector<constraint> entities;

    CompositeModel(const std::string &name);
    
    EntityPtr createEntity(const std::string& baseEntityName, std::string name = "");

    friend std::ostream &operator<<(std::ostream &stream, const CompositeModel &scene);
};


#endif //DRAWYMCDRAWFACE_SCENECONTAINER_H
