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

ParseTree::ParseTree(const String& parserOutput)
{
    std::stack<SentencePart*> parseStack;

    Stringstream strstr(parserOutput);

    String line;

    while (std::getline(strstr, line, '\n'))
    {
        size_t plus = line.find('+');

        unsigned int indent = (plus == String::npos) ? 0 : (plus + 4);

        unsigned int depth = indent / 4;

        size_t firstSpacePos = line.find(' ', indent);
        if (firstSpacePos == String::npos)
        {
            throw new RuntimeException(
                "Missing Parsey McParseface tag: \"" + line + "\"");
        }

        String word = line.substr(indent, firstSpacePos - indent);

        size_t secondSpacePos = line.find(' ', firstSpacePos + 1);
        if (secondSpacePos == String::npos)
        {
            throw new RuntimeException("Missing role tag: \"" + line + "\"");
        }

        String wordNatureTag = line.substr(firstSpacePos + 1,
                                                secondSpacePos - (firstSpacePos + 1));

        String wordRoleTag = line.substr(secondSpacePos + 1);

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

String ParseTree::getTreeAsString()
{
    Stringstream strstr;

    std::stack<std::pair<SentencePart*, int> > printStack;

    printStack.push(std::make_pair(&rootWord, 0));

    while (!printStack.empty())
    {
        SentencePart* word = printStack.top().first;
        int depth = printStack.top().second;

        printStack.pop();

        strstr << String(depth, '+') << word->getRootWord() << std::endl;

        for (auto itr = word->children.rbegin(); itr != word->children.rend();
                ++itr)
        {
            printStack.push(std::make_pair(&(*itr), depth + 1));
        }
    }

    return strstr.str();
}
