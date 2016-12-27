package nl.wernerkroneman.Drawy.ModelEditor;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

/**
 * A datastructure representing a part of a sentence.
 * This part is characterised by:
 *
 * - A root word with a corresponding grammatical nature and role
 * - A list of sub-parts.
 */
public class SentencePart {
    String nature;
    String role;
    String rootWord;
    List<SentencePart> children = new ArrayList<>();

    public SentencePart(String word,String tag,String role) {
        this.nature = tag;
        this.rootWord = word;
        this.role = role;
    }

    SentencePart dfsFind(Predicate<SentencePart> predicate) {

        if (predicate.test(this)) {
            return this;
        }

        for (SentencePart part : children)
        {
            SentencePart result = part.dfsFind(predicate);
            if (result != null)
                return result;
        }

        return null;
    }

    String getNature()
    {
        return nature;
    }

    void setNature(String nature)
    {
        this.nature = nature;
    }

    String getRole()
    {
        return role;
    }

    void setRole(String role)
    {
        this.role = role;
    }

    String getRootWord()
    {
        return rootWord;
    }

    void setRootWord(String rootWord)
    {
        this.rootWord = rootWord;
    }

    void addChild(SentencePart part)
    {
        children.add(part);
    }
}
