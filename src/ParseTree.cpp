/*
 * ParseTree.cpp
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

#include "ParseTree.h"

#include <assert.h>
#include <queue>
#include <stdexcept>

ParseTree::ParseTree(const std::string& parserOutput)
{
    std::stack<SentencePart*> parseStack;

    std::stringstream strstr(parserOutput);

    std::string line;

    while (std::getline(strstr, line, '\n'))
    {
        size_t plus = line.find('+');

        unsigned int indent = (plus == std::string::npos) ? 0 : (plus + 4);

        unsigned int depth = indent / 4;

        size_t firstSpacePos = line.find(' ', indent);
        if (firstSpacePos == std::string::npos)
        {
            throw std::runtime_error(
                "Missing Parsey McParseface tag: \"" + line + "\"");
        }

        std::string word = line.substr(indent, firstSpacePos - indent);

        size_t secondSpacePos = line.find(' ', firstSpacePos + 1);
        if (secondSpacePos == std::string::npos)
        {
            throw std::runtime_error("Missing role tag: \"" + line + "\"");
        }

        std::string wordNatureTag = line.substr(firstSpacePos + 1,
                                                secondSpacePos - (firstSpacePos + 1));

        std::string wordRoleTag = line.substr(secondSpacePos + 1);

        if (depth == 0)
        {
            rootWord = SentencePart(word, wordNatureTag ,wordRoleTag);
            parseStack.push(&rootWord);
        }
        else
        {
            while (depth < parseStack.size())
            {
                parseStack.pop();
            }
            assert(!parseStack.empty());

            parseStack.top()->addChild(SentencePart(word, wordNatureTag, wordRoleTag));

            parseStack.push(&parseStack.top()->children.back());
        }

    }

}

std::string ParseTree::getTreeAsString()
{
    std::stringstream strstr;

    std::stack<std::pair<SentencePart*, int> > printStack;

    printStack.push(std::make_pair(&rootWord, 0));

    while (!printStack.empty())
    {
        SentencePart* word = printStack.top().first;
        int depth = printStack.top().second;

        printStack.pop();

        strstr << std::string(depth, '+') << word->getRootWord() << std::endl;

        for (auto itr = word->children.rbegin(); itr != word->children.rend();
                ++itr)
        {
            printStack.push(std::make_pair(&(*itr), depth + 1));
        }
    }

    return strstr.str();
}
