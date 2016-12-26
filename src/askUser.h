#ifndef DRAWYMCDRAWFACE_ASKUSER_H
#define DRAWYMCDRAWFACE_ASKUSER_H

#include <string>
#include <iostream>

void tellUser(const std::string &question);

std::string askUserString(const std::string &question);

bool askUserYesNo(const std::string &question);

#endif //DRAWYMCDRAWFACE_ASKUSER_H
