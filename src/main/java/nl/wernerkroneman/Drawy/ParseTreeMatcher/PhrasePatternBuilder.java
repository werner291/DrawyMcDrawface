package nl.wernerkroneman.Drawy.ParseTreeMatcher;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Convenient PhraseTreePatternNode builder facilitating
 * an internal DSL to specify a PhraseTree pattern.
 */
public class PhrasePatternBuilder {
    private Pattern word;
    private Pattern nature;
    private Pattern role;
    private String name;
    private int repeat = 1;
    private List<PhrasePattern> children = null;

    public PhrasePatternBuilder setWord(Pattern word) {
        this.word = word;
        return this;
    }

    public PhrasePatternBuilder setNature(Pattern nature) {
        this.nature = nature;
        return this;
    }

    public PhrasePatternBuilder setRole(Pattern role) {
        this.role = role;
        return this;
    }


    public PhrasePatternBuilder setWord(String word) {
        this.word = Pattern.compile(word);
        return this;
    }

    public PhrasePatternBuilder setNature(String nature) {
        this.nature = Pattern.compile(nature);
        return this;
    }

    public PhrasePatternBuilder setRole(String role) {
        this.role = Pattern.compile(role);
        return this;
    }

    public PhrasePatternBuilder setName(String name) {
        this.name = name;
        return this;
    }

    public PhrasePatternBuilder setRepeat(int repeat) {
        this.repeat = repeat;
        return this;
    }

    public PhrasePatternBuilder setChildren(List<PhrasePattern> children) {
        this.children = children;
        return this;
    }

    public PhrasePatternBuilder setChildren(PhrasePattern... children) {
        this.children = Arrays.asList(children);
        return this;
    }

    public PhrasePatternBuilder addChild(PhrasePattern child) {
        if (this.children == null) {
            this.children = new ArrayList<>();
        }
        this.children.add(child);
        return this;
    }

    public PhrasePattern create() {
        return new PhrasePattern(word, nature, role, name, repeat, children);
    }

    public PhrasePatternBuilder setAnyChildren() {
        this.children = null;
        return this;
    }

    public PhrasePatternBuilder setNoChildren() {

        if (this.children == null) {
            this.children = new ArrayList<>();
        }

        this.children.clear();

        return this;
    }
}