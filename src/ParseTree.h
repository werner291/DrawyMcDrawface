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
    std::string nature;
    std::string role;
    std::string rootWord;
    std::vector<SentencePart> children;

    SentencePart(std::string word,std::string tag,std::string role)
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

    const std::string& getNature() const
    {
        return nature;
    }

    void setNature(const std::string& nature)
    {
        this->nature = nature;
    }

    const std::string& getRole() const
    {
        return role;
    }

    void setRole(const std::string& role)
    {
        this->role = role;
    }

    const std::string& getRootWord() const
    {
        return rootWord;
    }

    void setRootWord(const std::string& rootWord)
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
    ParseTree(const std::string& parserOutput);

    std::string getTreeAsString();

    virtual ~ParseTree() {}

    const SentencePart& getRootWord() const
    {
        return rootWord;
    }
};

#endif /* PARSETREE_H_ */
