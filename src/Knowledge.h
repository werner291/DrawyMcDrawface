//
// Created by werner on 25-12-16.
//

#ifndef DRAWYMCDRAWFACE_KNOWLEDGE_H
#define DRAWYMCDRAWFACE_KNOWLEDGE_H

#include <string>
#include <memory>

#include "CompositeModel.h"

struct comp {
    bool operator()(const std::string &lhs, const std::string &rhs) const {
        return stricmp(lhs.c_str(), rhs.c_str()) < 0;
    }
};

class Knowledge {

    std::map<std::string, std::shared_ptr<Model>, comp> knownObjects;

public:
    std::shared_ptr<Model> getObject(std::string name) {
        return knownObjects[name];
    }

    void remember(std::string name, std::shared_ptr<Model> model) {
        knownObjects.insert(std::make_pair(name, model));
    }

    friend std::ostream &operator<<(std::ostream &stream, const Model &scene) {

    }

};

void initializeKnowledgeWithPrimitives(Knowledge &knowledge);


#endif //DRAWYMCDRAWFACE_KNOWLEDGE_H
