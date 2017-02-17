package nl.wernerkroneman.Drawy.ParseTreeMatcher;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Convenient PhraseTreeNode builder facilitating
 * an internal DSL to specify a PhraseTree pattern.
 */
public class PhraseTreeNodeBuilder {
    private Pattern word;
    private Pattern nature;
    private Pattern role;
    private String name;
    private Class requiredSubpatternType;
    private int repeat = 1;
    private List<PhrasePattern.PhraseTreeNode> children = new ArrayList<>();

    public PhraseTreeNodeBuilder setWord(Pattern word) {
        this.word = word;
        return this;
    }

    public PhraseTreeNodeBuilder setNature(Pattern nature) {
        this.nature = nature;
        return this;
    }

    public PhraseTreeNodeBuilder setRole(Pattern role) {
        this.role = role;
        return this;
    }


    public PhraseTreeNodeBuilder setWord(String word) {
        this.word = Pattern.compile(word);
        return this;
    }

    public PhraseTreeNodeBuilder setNature(String nature) {
        this.nature = Pattern.compile(nature);
        return this;
    }

    public PhraseTreeNodeBuilder setRole(String role) {
        this.role = Pattern.compile(role);
        return this;
    }

    public PhraseTreeNodeBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public PhraseTreeNodeBuilder setRequiredSubpatternType(Class requiredSubpatternType) {
        this.requiredSubpatternType = requiredSubpatternType;
        return this;
    }

    public PhraseTreeNodeBuilder setRepeat(int repeat) {
        this.repeat = repeat;
        return this;
    }

    public PhraseTreeNodeBuilder setChildren(List<PhrasePattern.PhraseTreeNode> children) {
        this.children = children;
        return this;
    }

    public PhraseTreeNodeBuilder setChildren(PhrasePattern.PhraseTreeNode... children) {
        this.children = Arrays.asList(children);
        return this;
    }

    public PhraseTreeNodeBuilder addChild(PhrasePattern.PhraseTreeNode child) {
        this.children.add(child);
        return this;
    }

    public PhrasePattern.PhraseTreeNode createPhraseTreeNode() {
        return new PhrasePattern.PhraseTreeNode(word, nature, role, name, requiredSubpatternType, repeat, children);
    }
}