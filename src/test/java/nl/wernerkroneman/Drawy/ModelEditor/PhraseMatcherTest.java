package nl.wernerkroneman.Drawy.ModelEditor;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class PhraseMatcherTest {
    @Test
    public void match() throws Exception {
        
        PhraseMatcher pattern = PhraseMatcher.compilePattern("* * * hello");

        SentencePart phrase = new SentencePart("foo", "bar", "baz");

        PhraseMatcher.MatchResult result = pattern.match(phrase);

        Assert.assertTrue(result.matches);
        Assert.assertEquals(phrase, result.capturings.get("hello"));
    }

    @Test
    public void match2() throws Exception {

        PhraseMatcher pattern = PhraseMatcher.compilePattern("foo bar * hello");

        SentencePart phrase = new SentencePart("foo", "bar", "baz");

        PhraseMatcher.MatchResult result = pattern.match(phrase);

        Assert.assertTrue(result.matches);
        Assert.assertEquals(phrase, result.capturings.get("hello"));
    }

    @Test
    public void matchChild() throws Exception {

        PhraseMatcher pattern = PhraseMatcher.compilePattern("foo bar *\n" +
                " bar * baz child");

        SentencePart phrase = new SentencePart("foo", "bar", "baz");
        SentencePart child = new SentencePart("bar", "bar", "baz");
        phrase.addChild(child);

        PhraseMatcher.MatchResult result = pattern.match(phrase);

        Assert.assertTrue(result.matches);
        Assert.assertEquals(child, result.capturings.get("child"));
    }

    @Test
    public void matchGrandChild() throws Exception {

        PhraseMatcher pattern = PhraseMatcher.compilePattern("on * *\n" +
                " top * *\n" +
                "  of * *\n" +
                "   * * pobj relObj");

        SentencePart phrase = new SentencePart("on", "bar", "baz");
        SentencePart child = new SentencePart("top", "bar", "baz");
        phrase.addChild(child);
        SentencePart grandChild = new SentencePart("of", "bar", "baz");
        child.addChild(grandChild);
        SentencePart greatGrandChild = new SentencePart("car", "NN", "pobj");
        grandChild.addChild(greatGrandChild);

        PhraseMatcher.MatchResult result = pattern.match(phrase);

        Assert.assertTrue(result.matches);
        Assert.assertEquals(greatGrandChild, result.capturings.get("relObj"));
    }

    @Test
    public void matchFail() throws Exception {

        PhraseMatcher pattern = PhraseMatcher.compilePattern("foo bar * hello");

        SentencePart phrase = new SentencePart("foo", "ar", "baz");

        PhraseMatcher.MatchResult result = pattern.match(phrase);

        Assert.assertFalse(result.matches);
    }

}