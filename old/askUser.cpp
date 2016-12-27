#include "askUser.h"

String askUserString(const String &question) {

    std::cout << question << std::endl;
    std::cout << ">: " << std::endl;

    String line;

    std::getline(std::cin, line);

    return line;

}

bool askUserYesNo(const String &question) {

    while (true) {
        std::cout << question << std::endl;
        std::cout << "(Y/N): " << std::endl;

        String line;

        std::getline(std::cin, line);

        if (line == "Y") {
            return true;
        } else if (line == "N") {
            return false;
        }

        std::cout << "Invalid entry: " << line << std::endl;
    }

}

void tellUser(const String &toTell) {
    std::cout << toTell << std::endl;
}