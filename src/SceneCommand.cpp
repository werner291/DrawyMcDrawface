//
// Created by werner on 25-12-16.
//

#include "SceneCommand.h"

void SceneCommand::apply() {
    if (applied) {
        throw std::runtime_error("Trying to apply a statement that was already applied!");
    }

    onApply();

    applied = true;
}

void SceneCommand::revert() {
    if (!applied) {
        throw std::runtime_error("Trying to revert a non-applied statement.");
    }
    applied = false;
    onRevert();
}

void SceneCommand::onRevert() {
    throw std::runtime_error("This statement cannot be reverted.");
}

std::string CreateEntityRule::describe() {
    std::stringstream str;
    str << "Create " << number << " " << what << ".";
    return str.str();
}

void CreateEntityRule::onApply() {
    created = scene.lock()->createEntity(what);
}

CreateEntityRule::CreateEntityRule(const std::weak_ptr<CompositeModel> &scene) : SceneCommand(scene) { }

SceneCommand::SceneCommand(const std::weak_ptr<CompositeModel> &scene) : scene(scene) { }