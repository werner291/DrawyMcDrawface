//
// Created by werner on 25-12-16.
//

#ifndef DRAWYMCDRAWFACE_SCENERULE_H
#define DRAWYMCDRAWFACE_SCENERULE_H

#include <string>
#include <sstream>
#include "CompositeModel.h"

/**
 * An action to be applied on an CompositeModel.
 *
 * It follows a Command design pattern.
 *
 * Most commands are revertible.
 */
class SceneCommand {

protected:
    // Reference to the scene on which this statement is applied
    std::weak_ptr<CompositeModel> scene;
private:

    // Whether this command has been applied or not.
    bool applied = false;

public:
    SceneCommand(const std::weak_ptr<CompositeModel> &scene);

    virtual String describe() = 0;

    void apply();

    void revert();

    virtual void onApply() = 0;

    virtual void onRevert();
};

class CreateEntityRule : public SceneCommand {

public:
    // String describing what to create
    String what;

    // After applying, stores the entity that was created
    WeakEntityPtr created;

    // How many entities to create
    int number = 1;


    CreateEntityRule(const std::weak_ptr<CompositeModel> &scene);

    String describe();

    void onApply();

    //void onRevert() override {
    // TODO implement
    //    scene.lock()->deleteEntity(created);
    //    created.reset(nullptr);
    //}


};

#endif //DRAWYMCDRAWFACE_SCENERULE_H
