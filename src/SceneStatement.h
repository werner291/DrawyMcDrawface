//
// Created by werner on 25-12-16.
//

#ifndef DRAWYMCDRAWFACE_SCENERULE_H
#define DRAWYMCDRAWFACE_SCENERULE_H

#include <string>
#include <sstream>

class SceneStatement {
public:
    virtual std::string describe() = 0;
};

class CreateEntityRule : public SceneStatement {

public:
    std::string what;
    int number = 1;


    std::string describe() override {
        std::stringstream str;
        str << "Create " << number << " " << what << ".";
        return str.str();
    };
};

#endif //DRAWYMCDRAWFACE_SCENERULE_H
