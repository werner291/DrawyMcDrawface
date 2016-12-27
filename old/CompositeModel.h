//
// Created by werner on 11-6-16.
//

#ifndef DRAWYMCDRAWFACE_SCENECONTAINER_H
#define DRAWYMCDRAWFACE_SCENECONTAINER_H

#include <boost/container/flat_set.hpp>
#include <OGRE/OgreVector3.h>

class Model;

typedef std::weak_ptr<Model> WeakEntityPtr;
typedef Model> EntityPtr;

class Model {
  
  String name;

public:

    Model(const String &name) : name(name) { }
  
  const String& getName()
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

    PrimitiveModel(ShapeType shape, const String &name) : Model(name), shape(shape) { }
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
    std::vector<Model> > entities;
    //std::vector<constraint> entities;

    CompositeModel(const String &name);
    
    EntityPtr createEntity(const String& baseEntityName, String name = "");

    friend std::ostream &operator<<(std::ostream &stream, const CompositeModel &scene);
};


#endif //DRAWYMCDRAWFACE_SCENECONTAINER_H
