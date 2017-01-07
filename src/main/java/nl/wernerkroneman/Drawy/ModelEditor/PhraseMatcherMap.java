package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.Distance;

import java.util.ArrayList;
import java.util.List;

public class PhraseMatcherMap<ValueType> {

    class Entry {
        PhraseMatcher matcher;
        ValueType value;

        public Entry(PhraseMatcher matcher, ValueType value) {
            this.matcher = matcher;
            this.value = value;
        }
    }

    class EntryMatchResult {
        public EntryMatchResult(Entry entry, PhraseMatcher.MatchResult matchResult) {
            this.entry = entry;
            this.matchResult = matchResult;
        }

        Entry entry;
        PhraseMatcher.MatchResult matchResult;
    }

    List<Entry> entries = new ArrayList();



    void addPattern(PhraseMatcher matcher, ValueType value) {
        entries.add(new Entry(matcher, value));
    }

    EntryMatchResult findMatch(SentencePart phrase) {

        // Can perhaps be improved up to O(log n) through a rough ordering of the matchers.
        for (Entry entry : entries) {
            PhraseMatcher.MatchResult matchResult = entry.matcher.match(phrase);

            if (matchResult.matches) {
                return new EntryMatchResult(entry,matchResult);
            }
        }

        return null;
    }
}
