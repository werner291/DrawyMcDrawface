//
// Created by werner on 24-12-16.
//

#ifndef DRAWYMCDRAWFACE_SYNTAXNETLINK_H
#define DRAWYMCDRAWFACE_SYNTAXNETLINK_H

#include <iostream>
#include <string>
#include <memory>
#include "ParseTree.h"

std::string exec(const char* cmd);

class SyntaxNetLink {

public:
    ParseTree parse(const std::string& english);

};


#endif //DRAWYMCDRAWFACE_SYNTAXNETLINK_H
