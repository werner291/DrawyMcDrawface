//
// Created by werner on 25-12-16.
//

#include "CompositeModel.h"
#include "ParseTree.h"
#include <thread>
#include "workflow.h"
#include "askUser.h"
#include "Interpreter.h"

void showScene(std::shared_ptr<CompositeModel> scene);

std::shared_ptr<CompositeModel> descriptionSession(std::string subject, Knowledge &knowledge) {

    std::shared_ptr<CompositeModel> scene = std::make_shared<CompositeModel>("subject");

    std::string line;

    std::cout << subject << ">: ";

    while (getline(std::cin, line)) {

        if (line == "done") {
            break;
        }

        auto results = interpret(line, scene, knowledge, true);

        std::cout << "Interpreted: " << std::endl;

        for (auto stmt: results) {
            std::cout << stmt->describe() << std::endl;
        }

        if (results.empty()) {
            std::cout << "Unable to interpret. Guess I'm too stupid." << std::endl;
        }

        if (askUserYesNo("Is this correct?")) {
            for (auto cmd : results) {
                cmd->apply();
            }
        }

        showScene(scene);

        std::cout << subject << " >: ";
    }

    return scene;
}

void showScene(std::shared_ptr<CompositeModel> scene) {
    std::cout << "The scene currently looks like: " << std::endl;

    std::cout << *scene << std::endl;
}