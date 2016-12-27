//
// Created by werner on 24-12-16.
//

#include "SyntaxNetLink.h"

String exec(const char* cmd) {
    char buffer[128];
    String result = "";
    FILE> pipe(popen(cmd, "r"), pclose);
    if (!pipe) throw new RuntimeException("popen() failed!");
    while (!feof(pipe.get())) {
        if (fgets(buffer, 128, pipe.get()) != NULL)
            result += buffer;
    }
    return result;
}

ParseTree SyntaxNetLink::parse(const String& english) {

    if (std::any_of(english.begin(),english.end(), [](const char& c){
        return !(isalnum(c) || c == '.' || c == ',' || c == ' ');
    })) {
        throw new RuntimeException("String contains illegal or unsafe characters: " + english);
    }

    // TODO isn't it beautiful?
    String result = exec(("docker run 799d90a4425b bash -c \"echo '"+english+"' | syntaxnet/demo.sh\"").c_str());

    std::cerr << "---" << std::endl;

    // Cut off Input: and Parse: lines to leave the string itself.
    result.erase(0, result.find("\n") + 1);
    result.erase(0, result.find("\n") + 1);

    std::cerr << result << std::endl;

    return ParseTree(result);
}