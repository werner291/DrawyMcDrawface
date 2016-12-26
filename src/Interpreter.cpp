/*
 * Manipulator.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

#include "SceneCommand.h"
#include "Interpreter.h"
#include "SyntaxNetLink.h"
#include "askUser.h"
#include "workflow.h"

int interpretInteger(const std::string &text) ;

std::string pluralToSingular(const std::string &name) ;

bool isCreationVerb(const SentencePart &part);

int findNumber(const SentencePart& pPart);

void creationRuleForObject(std::vector<std::shared_ptr<SceneCommand>> &statements,
                           std::shared_ptr<CompositeModel> scene, const SentencePart &obj, Knowledge &know);

std::shared_ptr<Model> askUserWhatIs(const SentencePart &part, Knowledge &knowledge);

std::vector<std::shared_ptr<SceneCommand> > interpret(const std::string &toInterpret,
                                                      std::shared_ptr<CompositeModel> scene,
                                                      Knowledge &knowledge,
                                                      bool allowLearning) {

    std::vector<std::shared_ptr<SceneCommand> > statements;

    SyntaxNetLink linky;

    ParseTree tree = linky.parse(toInterpret);

    const SentencePart& rootWord = tree.getRootWord();

    if (rootWord.getNature() != "VB" || isCreationVerb(rootWord)) {
        // This is a command in the form of "Create a cube", or simply "a cube"

        const SentencePart *obj = rootWord.dfsFind([](const SentencePart &part) {
            return boost::starts_with(part.getNature(), "NN");
        });

        if (obj == nullptr) {
            throw std::runtime_error("Cannot find object to create.");
        }

        creationRuleForObject(statements, scene, *obj, knowledge);
    }

    return statements;
}

/**
 * Generate a creation command for the object(s) described in this sentence part.
 */
void creationRuleForObject(std::vector<std::shared_ptr<SceneCommand>> &statements,
                           std::shared_ptr<CompositeModel> scene,
                           const SentencePart &obj,
                           Knowledge &know) {

    std::shared_ptr<CreateEntityRule> createStmt = std::make_shared<CreateEntityRule>(scene);

    if (obj.getNature() == "NNS") {
        // Plural
        createStmt->number = findNumber(obj);
        createStmt->what = pluralToSingular(obj.getRootWord());
    } else {
        // Singular
        createStmt->number = 1;
        createStmt->what = obj.getRootWord();
    }

    std::shared_ptr<Model> toCreate = know.getObject(createStmt->what);

    if (toCreate.get() == nullptr) {
        toCreate = askUserWhatIs(obj, know);
    }

    statements.push_back(createStmt);

    for (auto itr = obj.children.begin(); itr != obj.children.end(); ++itr) {
        // Look for "and something else"-type phrases.
        if (itr->getRole() == "cc") {
            if (itr->getRootWord() != "and") {
                // This is an "or" or "while", don't know how to handle those yet!
                throw std::runtime_error("I don't know how to handle coordinating conjunction "
                                         + itr->getRootWord() + " yet, sorry!");
            }

            // Add separate creation rules for the conjuncts as well.
            creationRuleForObject(statements, scene, *(++itr), know);
        }
    }
}

std::shared_ptr<Model> askUserWhatIs(const SentencePart &part, Knowledge &knowledge) {
    if (askUserYesNo("I don't know what '" + part.getRootWord() + "' is. Can you tell me?")) {

        tellUser("Ok, please tell me about " + part.getRootWord());

        auto model = descriptionSession(part.getRootWord(), knowledge);

        if (askUserYesNo("Would you like me to remember this?")) {
            knowledge.remember(part.getRootWord(), model);
        } else {
            tellUser("Ok, I'll only use this description here. ");
        }

        tellUser("TIL what " + part.getRootWord() + " looks like.");
    } else {
        tellUser("Ok... I guess?");
        throw std::runtime_error("Uncooperative user.");
    }
}

/**
 * Tries to find some indication of a number of items.
 * For example, in a phrase "15 lions", this function would return 15.
 */
int findNumber(const SentencePart& pPart) {
    const SentencePart* numeric = pPart.dfsFind([](const SentencePart& part){return part.getNature() == "CD";});

    if (numeric == nullptr) {
        throw std::runtime_error("Cannot find a number of " + pPart.getRootWord());
    }

    return interpretInteger(numeric->getRootWord());
}

/**
 * Returns whether this part of the sencence is a verb
 * that commands creating somehting such as "create" or "add".
 */
bool isCreationVerb(const SentencePart &part) {
    if (part.getNature() != "VB") return false;

    const std::string &word = part.getRootWord();

    return boost::iequals(word,"create") || boost::iequals(word,"add");
}

int interpretInteger(const std::string &text) {
    char *p;
    int result = (int) strtol(text.c_str(), &p, 10);

    return result;
}

/**
 * Take a plural form of a word, and make it singular.
 * Mainly designed to work on nouns.
 *
 * For example, it converts "parties" to "party".
 */
std::string pluralToSingular(const std::string &name) {
    // TODO do this properly
    if (boost::algorithm::ends_with(name, "ies")) {
        return name.substr(0, name.length() - 3) + "y";
    }

    if (boost::algorithm::ends_with(name, "s")) {
        return name.substr(0, name.length() - 1);
    }

    throw std::runtime_error("Cannot de-pluralize " + name);
}
