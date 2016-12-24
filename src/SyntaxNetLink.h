//
// Created by werner on 24-12-16.
//

#ifndef DRAWYMCDRAWFACE_SYNTAXNETLINK_H
#define DRAWYMCDRAWFACE_SYNTAXNETLINK_H


#include <string>
#include "ParseTree.h"

class SyntaxNetLink {

    ParseTree parse(const std::string& english) {

        if (std::any_of(english.begin(),english.end(), [](const char& c){
            return !(isalnum(c) || c == '.' || c == ',');
        })) {
            throw std::runtime_error("String contains illegal or unsafe characters: " + english);
        }

        system("docker run 799d90a4425b bash -c \"echo '"+english+"' | syntaxnet/demo.sh\"");
    }

};


#endif //DRAWYMCDRAWFACE_SYNTAXNETLINK_H
