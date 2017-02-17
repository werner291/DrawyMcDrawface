package nl.wernerkroneman.Drawy.ParseTreeMatcher;

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
public class PhraseTree {

    PhraseTree parent;

    String nature;
    String role;
    String rootWord;
    List<PhraseTree> children = new ArrayList<>();

    public PhraseTree(String word, String tag, String role) {
        this.nature = tag;
        this.rootWord = word;
        this.role = role;
    }

    public PhraseTree dfsFind(Predicate<PhraseTree> predicate) {

        if (predicate.test(this)) {
            return this;
        }

        for (PhraseTree part : children)
        {
            PhraseTree result = part.dfsFind(predicate);
            if (result != null)
                return result;
        }

        return null;
    }

    public PhraseTree findFirstChild(Predicate<PhraseTree> predicate) {

        for (PhraseTree part : children) {
            PhraseTree result = part.dfsFind(predicate);
            if (result != null)
                return result;
        }

        return null;
    }

    public String getNature()
    {
        return nature;
    }

    void setNature(String nature)
    {
        this.nature = nature;
    }

    public String getRole()
    {
        return role;
    }

    void setRole(String role)
    {
        this.role = role;
    }

    public String getRootWord()
    {
        return rootWord;
    }

    void setRootWord(String rootWord)
    {
        this.rootWord = rootWord;
    }

    public void addChild(PhraseTree part)
    {
        if (part.parent != null) {
            throw new IllegalStateException("PhraseTree may not be a child to more than one parent.");
        }
        part.parent = this;
        children.add(part);
    }

    public PhraseTree deepCopy() {
        PhraseTree copy = new PhraseTree(this.rootWord, this.nature, this.role);

        for (PhraseTree child : children) {
            copy.children.add(child.deepCopy());
        }

        return copy;
    }

    @Override
    public String toString() {
        return "PhraseTree{" +
                ", nature='" + nature + '\'' +
                ", role='" + role + '\'' +
                ", rootWord='" + rootWord + '\'' +
                ", children=" + children +
                '}';
    }

    public List<PhraseTree> getChildren() {
        return children;
    }
}
