//
// Created by werner on 24-12-16.
//

#include "SyntaxNetLink.h"

std::string exec(const char* cmd) {
    char buffer[128];
    std::string result = "";
    std::shared_ptr<FILE> pipe(popen(cmd, "r"), pclose);
    if (!pipe) throw std::runtime_error("popen() failed!");
    while (!feof(pipe.get())) {
        if (fgets(buffer, 128, pipe.get()) != NULL)
            result += buffer;
    }
    return result;
}

ParseTree SyntaxNetLink::parse(const std::string& english) {

    if (std::any_of(english.begin(),english.end(), [](const char& c){
        return !(isalnum(c) || c == '.' || c == ',' || c == ' ');
    })) {
        throw std::runtime_error("String contains illegal or unsafe characters: " + english);
    }

    // TODO isn't it beautiful?
    std::string result = exec(("docker run 799d90a4425b bash -c \"echo '"+english+"' | syntaxnet/demo.sh\"").c_str());

    std::cerr << "---" << std::endl;

    // Cut off Input: and Parse: lines to leave the string itself.
    result.erase(0, result.find("\n") + 1);
    result.erase(0, result.find("\n") + 1);

    std::cerr << result << std::endl;

    return ParseTree(result);
}