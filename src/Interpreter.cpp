/*
 * Manipulator.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

#include "Interpreter.h"
#include <boost/algorithm/string/predicate.hpp>

Interpreter::Interpreter(SceneModel& sceneContainer)
    : sceneContainer(sceneContainer) {
    // TODO Auto-generated constructor stub

}

Interpreter::~Interpreter() {
    // TODO Auto-generated destructor stub
}

bool Interpreter::interpretParsed(const ParseTree& parsetree) {

    const SentencePart& rootWord = parsetree.getRootWord();

    // Check if the root word is a noun (as opposed to a verb)
    bool rootWordIsNoun = boost::algorithm::starts_with(rootWord.getNature(), "NN");

    // Check for existential statements (like "There is")
    bool containsExistentialStatement =
        rootWord.dfsFind([](const SentencePart& part)
    {
        return part.getNature() == "EX";
    }) != nullptr;

    // No verb or an existential implies object creation
    if (rootWordIsNoun || containsExistentialStatement)
    {
        std::cout << "Will create object." << std::endl;

        // Find the subject of the sentence
        const SentencePart* toCreate = rootWord.dfsFind([](const SentencePart& part) {
            return boost::algorithm::starts_with(part.getNature(), "NN");
        });

        if (toCreate == nullptr)
        {
            std::cerr << "No NN found to create." << std::endl;
        }

        // By default, create just 1
        int number = 1;

        // NNS indicates plural.
        bool plural = toCreate->getNature() == "NNS";

        // Name of the entity
        std::string name = toCreate->getRootWord();

        if (plural)
        {

            std::cout << "Detected plural statement" << std::endl;

            // Convert the name to singular form
            std::pair<std::string, bool> singularResult = pluralToSingular(name);

            if (!singularResult.second)
            {
                std::cout << "Cannot determine singular form of " << singularResult.first << std::endl;
                return false;
            }

            name = singularResult.first;

            // Check whether the user gave any kind of numeric statement.
            const SentencePart* numeric = rootWord.dfsFind([](const SentencePart& part) {
                std::cout << "<=>" << part.getRole() << std::endl;
                return part.getNature() == "CD";
            });

            // Found an actual number! Let's see if we can interpret it.
            if (numeric != nullptr)
            {
                std::pair<int,float> interpretation = interpretInteger(numeric->getRootWord());

                if (interpretation.second < 0.5f)
                {
                    // Too unsure.
                    std::cout << "I'm not sure what value \""
                              << numeric->getRootWord()
                              << "\" is. Interpreting it as "
                              << number
                              << "." << std::endl;
                }
                else
                {
                    number = interpretation.first;
                }

                if (number > 100)
                {
                    // More than 100 entities at a time is probably not what we want.
                    // Should we prompt the user?
                    std::cout << "Limiting group size to 100 units." << std::endl;
                    number = 100;
                }
            }
            else
            {
                number = 4; // What's a good interpretation of "a few"?
            }


        }
        else
        {

        }

        for (int i = 0; i < number; ++i)
	{
	  EntityPtr newEnt = sceneContainer.createEntity(name);
	  
	  sceneContainer.addEntityToGroup(newEnt->getName(), "Current Scene");
	}
    }

    return true;
}

std::pair< int, float > Interpreter::interpretInteger(const std::string& text)
{
    char* p;
    int result = strtol(text.c_str(), &p, 10);

    std::make_pair(result, p == nullptr ? 0.f : 1.f);
}

std::pair<std::string, bool> Interpreter::pluralToSingular(const std::string& name)
{
    // TODO do this properly
    if (boost::algorithm::ends_with(name, "ies"))
    {
        return std::make_pair(name.substr(0,name.length()-3) + "y", true);
    }

    if (boost::algorithm::ends_with(name, "s"))
    {
        return std::make_pair(name.substr(0,name.length()-1), true);
    }

    return std::make_pair(name,false);
}
