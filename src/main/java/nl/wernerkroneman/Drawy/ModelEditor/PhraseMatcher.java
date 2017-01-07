package nl.wernerkroneman.Drawy.ModelEditor;

import java.util.*;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * A class that allows pattern matching against SentenceParts.
 * A bit like regexes.
 */
public class PhraseMatcher {

    Predicate<String> wordMatch;
    Predicate<String> natureMatch;
    Predicate<String> roleMatch;
    String matchKey;

    Collection<PhraseMatcher> children = new ArrayList<>();

    /**
     * Create a pattern based on the specified pattern code.
     *
     * It is in the form of:
     *
     * word nature role [matchKey]
     *  childWord childWord childRole [childMatchKey]
     *   grandChildWord grandChildWord grandChildRole [grandChildMatchKey]
     *  child2Word child2Word child2Role [child2MatchKey]
     *
     * One word on each line, children are indented in a number of spaces.
     * matchKey is optional, and can be used to locate certain phrases (like capturing groups).
     *
     * @param matchPattern The pattern as a string in the pattern matching DSL.
     * @return PhraseMatcher
     */
    public static PhraseMatcher compilePattern(String matchPattern) {
        return new PhraseMatcher(Arrays.asList(matchPattern.split("\n")));
    }

    PhraseMatcher(List<String> lines) {
        String[] tokens = lines.get(0).split(" ");

        if (tokens.length < 3) {
            throw new IllegalArgumentException("Cannot parse line, must have at least 3: " + tokens + "(use * to match any)");
        }

        wordMatch = tokens[0].equals("*") ? a->true : Pattern.compile(tokens[0]).asPredicate();
        natureMatch = tokens[1].equals("*") ? a->true : Pattern.compile(tokens[1]).asPredicate();
        roleMatch = tokens[2].equals("*") ? a->true : Pattern.compile(tokens[2]).asPredicate();
        if (tokens.length >= 4) {
            // If available, get the matchKey.
            matchKey = tokens[3];
        }

        // Cut up the child lines into child matcherz
        List<String> childLines = new ArrayList<>();

        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);

            childLines.add(line.substring(1));

            // Look ahead. If this is the last line or the next line has a single leading space
            if (i == lines.size()-1 || (lines.get(i+1).charAt(0) == ' ' && lines.get(i+1).length() >= 2 && lines.get(i+1).charAt(1) != ' ')) {
                // Turn the lines accumulated to far into a PhraseMatcher and clear the accumulator
                children.add(new PhraseMatcher(childLines));
                childLines.clear();
            }
        }
    }

    public static class MatchResult {
        Map<String, SentencePart> capturings;
        boolean matches;
    }

    MatchResult match(SentencePart matchWith) {
        MatchResult result = new MatchResult();
        result.capturings = new HashMap<>();
        result.matches = matchImpl(matchWith, result.capturings);
        return result;
    }

    boolean matchImpl(SentencePart matchWith, final Map<String,SentencePart> resultMapper) {

        if (!(wordMatch.equals("*") || wordMatch.test(matchWith.getRootWord()))) {
            return false;
        } else if (!(natureMatch.equals("*") || natureMatch.test(matchWith.getNature()))) {
            return false;
        } else if (!(roleMatch.equals("*") || roleMatch.test(matchWith.getRole()))) {
            return false;
        }

        if (matchKey != null) {
            resultMapper.put(matchKey, matchWith);
        }

        return children.stream().allMatch(pc ->
                matchWith.children.stream().anyMatch(mc-> pc.matchImpl(mc,resultMapper)));
    }

}
