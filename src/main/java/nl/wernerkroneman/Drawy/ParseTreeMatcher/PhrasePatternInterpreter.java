package nl.wernerkroneman.Drawy.ParseTreeMatcher;

import java.util.*;
import java.util.function.Function;

/**
 * A structure containing a phrase tree pattern,
 * the class represented by that pattern,
 * and the factory function that produces the object.
 */
public interface PhrasePatternInterpreter<T> {

    /*Set<PhrasePattern> patterns;

    public T interpret(PhraseTree phrase) {

        for (PatternEntry entry : entries) {

            PhrasePattern.MatchResult matchResult = entry.pattern.matchAgainst(phrase);

            if (matchResult.matches) {
                return entry.value.apply(matchResult.capturings);
            }
        }

        return null;
    }*/



}