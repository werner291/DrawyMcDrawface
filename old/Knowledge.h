//
// Created by werner on 25-12-16.
//

#ifndef DRAWYMCDRAWFACE_KNOWLEDGE_H
#define DRAWYMCDRAWFACE_KNOWLEDGE_H

#include <string>
#include <memory>

#include "CompositeModel.h"

struct comp {
    bool operator()(const String &lhs, const String &rhs) const {
        return stricmp(lhs.c_str(), rhs.c_str()) < 0;
    }
};

class Knowledge {

    std::map<String, Model>, comp> knownObjects;

public:
    Model> getObject(String name) {
        return knownObjects[name];
    }

    void remember(String name, Model> model) {
        knownObjects.insert(std::make_pair(name, model));
    }

    friend std::ostream &operator<<(std::ostream &stream, const Model &scene) {

    }

    int getNumberOfObjects() {
        return knownObjects.size();
    }

};

void initializeKnowledgeWithPrimitives(Knowledge &knowledge);


#endif //DRAWYMCDRAWFACE_KNOWLEDGE_H
