package nl.wernerkroneman.Drawy.ParseTreeMatcher;

import com.sun.org.apache.xerces.internal.impl.xpath.regex.Match;

import java.util.*;
import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * A structure containing a phrase tree pattern,
 * the class represented by that pattern,
 * and the factory function that produces the object.
 */
public class PhrasePattern {

    Class representsType;

    PhraseTreeNode patternRoot;

    public PhrasePattern(Class representsType,
                         PhraseTreeNode patternRoot,
                         Function<MatchResult, Object> resultProcessor) {
        this.representsType = representsType;
        this.patternRoot = patternRoot;
        this.resultProcessor = resultProcessor;
    }

    public MatchResult matchAgainst(PhraseTree phrase) {

        MatchResult result = new MatchResult();

        result.matches = patternRoot.matchAgainst(phrase, result);

        return result;

    }

    public static class MatchResult {
        boolean matches;
        int matchScore;
        Map<String, PhraseTree> capturings = new HashMap<>();
        public Map<String, Object> dependencies = new HashMap<>();
    }

    Function<PhrasePattern.MatchResult, ? extends Object> resultProcessor;

    /**
     * Represents a node in a phrase tree pattern,
     * and the root of a sub-tree.
     */
    static class PhraseTreeNode {
        Pattern word, nature, role;
        String name;
        Class requiredSubpatternType = null;
        int repeat = 1;

        List<PhraseTreeNode> children;

        public PhraseTreeNode(Pattern word,
                              Pattern nature,
                              Pattern role,
                              String name,
                              Class requiredSubpatternType,
                              int repeat,
                              List<PhraseTreeNode> children) {
            this.word = word;
            this.nature = nature;
            this.role = role;
            this.name = name;
            this.requiredSubpatternType = requiredSubpatternType;
            this.repeat = repeat;
            this.children = children;

            // Check invariants:
            if (name == null && requiredSubpatternType != null) {
                throw new IllegalArgumentException("Anonymous dependencies not allowed");
            }

            if (requiredSubpatternType != null &&
                    (this.children != null && !this.children.isEmpty())) {
                throw new IllegalArgumentException("Phrase tree node cannot both have" +
                        " children and represent a dependency.");
            }
        }

        public boolean matchAgainst(PhraseTree phrase, MatchResult result) {

            if ((word == null || word.matcher(phrase.getRootWord()).find()) &&
                    (nature == null || nature.matcher(phrase.getNature()).find()) &&
                            (role == null || role.matcher(phrase.getRole()).find())) {

                if (name != null) {
                    result.capturings.put(name, phrase);
                    if (requiredSubpatternType != null) {
                        result.dependencies.put(name,phrase);
                    }
                }

                ListIterator<PhraseTree> phraseItr = phrase.children.listIterator();

                if (this.children.isEmpty() && !phrase.children.isEmpty()){
                    return false;
                }

                for (PhraseTreeNode pattern : this.children) {

                    for (int copies = pattern.repeat; copies > 0; copies--) {

                        if (!phraseItr.hasNext()) {
                            return false;
                        } else {
                            PhraseTree phraseChild = phraseItr.next();

                            if (!pattern.matchAgainst(phraseChild,result)){
                                return false;
                            }
                        }

                    }

                }

                return true;
            } else {
                return false;
            }

        }
    }

}