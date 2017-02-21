package nl.wernerkroneman.Drawy.ParseTreeMatcher;

import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Represents a node in a phrase tree pattern,
 * and the root of a sub-tree.
 */
public class PhrasePattern {

    public static class MatchResult {
        public boolean matches;
        public Map<String, PhraseTree> capturings = new HashMap<>();
    }

    // Regexes for the word, nature and role
    // Null means any will do.
    Pattern word, nature, role;

    // If not null, MatchResults capturings will include reference
    // to phrase tree under this key name
    String name;

    // How many times to repeat this pattern.
    // Note: meaningless on root pattern
    int repeat = 1;

    // List of child patterns.
    // If null, ignore any children.
    // If not null, children must match exactly.
    // Empty list means no children allowed.
    List<PhrasePattern> children;

    public PhrasePattern(Pattern word,
                         Pattern nature,
                         Pattern role,
                         String name,
                         int repeat,
                         List<PhrasePattern> children) {
        this.word = word;
        this.nature = nature;
        this.role = role;
        this.name = name;
        this.repeat = repeat;
        this.children = children;
    }

    public MatchResult matchAgainst(PhraseTree phrase) {
        MatchResult matchResult = new MatchResult();

        matchResult.matches = this.matchAgainstImpl(phrase, matchResult);

        return matchResult;
    }

    protected boolean matchAgainstImpl(PhraseTree phrase, MatchResult result) {

        // First, verify that the word, nature and role match the regexes
        if ((word == null || word.matcher(phrase.getRootWord()).find()) &&
                (nature == null || nature.matcher(phrase.getNature()).find()) &&
                        (role == null || role.matcher(phrase.getRole()).find())) {

            // If necessary, capture
            if (name != null) {
                result.capturings.put(name, phrase);
            }

            // If information about children is present,
            // match the children
            if (this.children != null) {
                if (!matchChildren(phrase, result)) {
                    return false;
                }
            }

            // Matches!
            return true;
        } else {
            return false;
        }

    }

    // Match the children with each other
    private boolean matchChildren(PhraseTree phrase, MatchResult result) {
        // Get an iterator over the children of the phrase
        ListIterator<PhraseTree> phraseItr = phrase.children.listIterator();

        // If no children are allowed, but it has children, fail test
        if (this.children.isEmpty() && !phrase.children.isEmpty()) {
            return false;
        }

        // Try to consume all children with the child patterns
        for (PhrasePattern pattern : this.children) {

            // Repeat for required number of times
            for (int copies = pattern.repeat; copies > 0; copies--) {

                if (!phraseItr.hasNext()) {
                    // There should be a child, but there isn't.
                    return false;
                } else {
                    PhraseTree phraseChild = phraseItr.next();

                    // Recursive matching
                    if (!pattern.matchAgainstImpl(phraseChild, result)) {
                        return false;
                    }
                }

            }

        }

        // All children should have been consumed.
        return !phraseItr.hasNext();
    }
}
