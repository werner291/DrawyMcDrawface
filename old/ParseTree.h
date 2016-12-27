/*
 * ParseTree.h
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */

#ifndef PARSETREE_H_
#define PARSETREE_H_

#include <string>
#include <vector>
#include <sstream>
#include <stack>
#include <iostream>
#include <functional>
#include <boost/algorithm/string/predicate.hpp>

//#include "recursive_iterator.hpp"

/**
 * A datastructure representing a part of a sentence.
 * This part is characterised by:
 *
 * - A root word with a corresponding grammatical nature and role
 * - A list of sub-parts.
 */
class SentencePart
{

public:
    String nature;
    String role;
    String rootWord;
    std::vector<SentencePart> children;

    SentencePart(String word,String tag,String role)
        : nature(tag), rootWord(word), role(role) {}

    SentencePart(const SentencePart& word)
        : nature(word.nature), rootWord(word.rootWord), role(word.role), children(word.children) {}

    SentencePart() : nature("UNSET"), rootWord("") {}

    const SentencePart* dfsFind(const std::function<bool(const SentencePart&)>& predicate) const
    {
        
        if (predicate(*this))
            return this;

        for (const SentencePart& part : children)
        {
            const SentencePart* result = part.dfsFind(predicate);
            if (result != nullptr)
                return result;
        }

        return nullptr;
    }

    const String& getNature() const
    {
        return nature;
    }

    void setNature(const String& nature)
    {
        this->nature = nature;
    }

    const String& getRole() const
    {
        return role;
    }

    void setRole(const String& role)
    {
        this->role = role;
    }

    const String& getRootWord() const
    {
        return rootWord;
    }

    void setRootWord(const String& rootWord)
    {
        this->rootWord = rootWord;
    }

    void addChild(const SentencePart& part)
    {
        children.push_back(part);
    }
};

class ParseTree
{
    SentencePart rootWord;

public:
    ParseTree(const String& parserOutput);

    String getTreeAsString();

    virtual ~ParseTree() {}

    const SentencePart& getRootWord() const
    {
        return rootWord;
    }
};

#endif /* PARSETREE_H_ */
