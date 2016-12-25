#include "askUser.h"

std::string askUserString(const std::string &question) {

    std::cout << question << std::endl;
    std::cout << ">: " << std::endl;

    std::string line;

    std::getline(std::cin, line);

    return line;

}

bool askUserYesNo(const std::string &question) {

    while (true) {
        std::cout << question << std::endl;
        std::cout << "(Y/N): " << std::endl;

        std::string line;

        std::getline(std::cin, line);

        if (line == "Y") {
            return true;
        } else if (line == "F") {
            return false;
        }

        std::cout << "Invalid entry: " << line << std::endl;
    }

}