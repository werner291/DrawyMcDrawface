package nl.wernerkroneman.Drawy.ParseTreeMatcher;


import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

/**
 * Main class of the parse tree matching framework.
 */
public class MainMatcher {

    List<PhrasePattern> library = new ArrayList<>();

    /**
     * Add a pattern and corresponding processor.
     *
     * @param pattern The phrase matcher
     * @param
     */
    public void addPattern(PhrasePattern pattern) {
        library.add(pattern);
    }

    public Object match(PhraseTree phrase) {

        return match(phrase, PhrasePattern -> true);

    }

    public Object match(PhraseTree phrase, Predicate<PhrasePattern> filter) {

        for (PhrasePattern pattern : library) {
            if (filter.test(pattern)) {

                PhrasePattern.MatchResult result = pattern.matchAgainst(phrase);

                if (result.matches) {
                    return pattern.resultProcessor.apply(result);
                }

            }
        }

        return null;
    }

}
