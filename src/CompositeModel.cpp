//
// Created by werner on 11-6-16.
//

#include "CompositeModel.h"

CompositeModel::CompositeModel(const std::string &name) : Model(name) {
}

EntityPtr CompositeModel::createEntity(const std::string &baseEntityName, std::string name) {

    if (name.empty())
    {
        name = std::to_string(idGen++) + baseEntityName;
    }

    std::shared_ptr<Model> newEntity(new Model(name));

    entities.push_back(newEntity);
    
    return newEntity;
}

std::ostream &operator<<(std::ostream &stream, const CompositeModel &scene) {
    stream << "Entities:" << std::endl;
    for (auto ent : scene.entities) {
        stream << " - " << ent->getName() << std::endl;
    }
    return stream;
}