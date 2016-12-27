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

int interpretInteger(const String &text) ;

String pluralToSingular(const String &name) ;

bool isCreationVerb(const SentencePart &part);

int findNumber(const SentencePart& pPart);

void creationRuleForObject(std::vector<SceneCommand>> &statements,
                           CompositeModel> scene,
                           const SentencePart &obj,
                           Knowledge &know);

Model> askUserWhatIs(const SentencePart &part, Knowledge &knowledge);

std::vector<SceneCommand> > interpret(const String &toInterpret,
                                                      CompositeModel> scene, Knowledge &knowledge) {

    std::vector<SceneCommand> > statements;

    SyntaxNetLink linky;

    ParseTree tree = linky.parse(toInterpret);

    const SentencePart& rootWord = tree.getRootWord();

    if (rootWord.getNature() != "VB" || isCreationVerb(rootWord)) {
        // This is a command in the form of "Create a cube", or simply "a cube"

        const SentencePart *obj = rootWord.dfsFind([](const SentencePart &part) {
            return boost::starts_with(part.getNature(), "NN");
        });

        if (obj == nullptr) {
            throw new RuntimeException("Cannot find object to create.");
        }

        creationRuleForObject(statements, scene, *obj, knowledge);
    }

    return statements;
}

/**
 * Generate a creation command for the object(s) described in this sentence part.
 */
void creationRuleForObject(std::vector<SceneCommand>> &statements,
                           CompositeModel> scene,
                           const SentencePart &obj,
                           Knowledge &know) {

    CreateEntityRule> createStmt = std::make_shared<CreateEntityRule>(scene);

    if (obj.getNature() == "NNS") {
        // Plural
        createStmt->number = findNumber(obj);
        createStmt->what = pluralToSingular(obj.getRootWord());
    } else {
        // Singular
        createStmt->number = 1;
        createStmt->what = obj.getRootWord();
    }

    Model> toCreate = know.getObject(createStmt->what);

    if (toCreate.get() == nullptr) {
        toCreate = askUserWhatIs(obj, know);
    }

    statements.push_back(createStmt);

    for (auto itr = obj.children.begin(); itr != obj.children.end(); ++itr) {
        // Look for "and something else"-type phrases.
        if (itr->getRole() == "cc") {
            if (itr->getRootWord() != "and") {
                // This is an "or" or "while", don't know how to handle those yet!
                throw new RuntimeException("I don't know how to handle coordinating conjunction "
                                         + itr->getRootWord() + " yet, sorry!");
            }

            // Add separate creation rules for the conjuncts as well.
            creationRuleForObject(statements, scene, *(++itr), know);
        }
    }
}

/**
 * Tries to find some indication of a number of items.
 * For example, in a phrase "15 lions", this function would return 15.
 */
int findNumber(const SentencePart& pPart) {
    const SentencePart* numeric = pPart.dfsFind([](const SentencePart& part){return part.getNature() == "CD";});

    if (numeric == nullptr) {
        throw new RuntimeException("Cannot find a number of " + pPart.getRootWord());
    }

    return interpretInteger(numeric->getRootWord());
}

/**
 * Returns whether this part of the sencence is a verb
 * that commands creating somehting such as "create" or "add".
 */
bool isCreationVerb(const SentencePart &part) {
    if (part.getNature() != "VB") return false;

    const String &word = part.getRootWord();

    return boost::iequals(word,"create") || boost::iequals(word,"add");
}

int interpretInteger(const String &text) {
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
String pluralToSingular(const String &name) {
    // TODO do this properly
    if (boost::algorithm::ends_with(name, "ies")) {
        return name.substr(0, name.length() - 3) + "y";
    }

    if (boost::algorithm::ends_with(name, "s")) {
        return name.substr(0, name.length() - 1);
    }

    throw new RuntimeException("Cannot de-pluralize " + name);
}
