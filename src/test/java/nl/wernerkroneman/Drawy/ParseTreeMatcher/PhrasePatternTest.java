package nl.wernerkroneman.Drawy.ParseTreeMatcher;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by werner on 17-2-17.
 */
public class PhrasePatternTest {

    @Test
    public void matchAgainst() throws Exception {

        PhrasePattern pattern =  new PhrasePatternBuilder()
                        .setWord("Hello")
                        .setNoChildren()
                        .create();

        PhraseTree phrase = new PhraseTree("Hello","NN", "root");

        assertTrue(pattern.matchAgainst(phrase).matches);

        phrase.addChild(new PhraseTree("world", "NN", "nobj"));

        assertFalse(pattern.matchAgainst(phrase).matches);

    }

    @Test
    public void matchAgainstPatternWithChild() throws Exception {

        PhrasePattern pattern = new PhrasePatternBuilder()
                        .setWord("Hello")
                        .setChildren(
                                new PhrasePatternBuilder()
                                        .setWord("world")
                                        .create()
                        ).create();

        PhraseTree phrase = new PhraseTree("Hello","NN", "root");
        phrase.addChild(new PhraseTree("world", "NN", "nobj"));

        assertTrue(pattern.matchAgainst(phrase).matches);
    }

    @Test
    public void matchAgainstPatternWithChildAndDependency() throws Exception {

        PhrasePattern pattern = new PhrasePatternBuilder()
                .setWord("Hello")
                .setChildren(
                        new PhrasePatternBuilder()
                                .setWord("world")
                                .create(),
                        new PhrasePatternBuilder()
                                .setName("testdependency")
                                .create()
                ).create();

        PhraseTree phrase = new PhraseTree("Hello","NN", "root");
        phrase.addChild(new PhraseTree("world", "NN", "nobj"));
        phrase.addChild(new PhraseTree("bar", "NN", "nobj"));

        PhrasePattern.MatchResult matchResult = pattern.matchAgainst(phrase);
        assertTrue(matchResult.matches);

        assertEquals(1,matchResult.capturings.size());
        assertEquals("bar",matchResult.capturings.get("testdependency").getRootWord());
    }

    @Test
    public void matchAgainstPatternAnychildNochild() throws Exception {

        PhrasePattern anychild = new PhrasePatternBuilder()
                .setWord("Hello")
                .setAnyChildren()
                .create();

        PhrasePattern noChild = new PhrasePatternBuilder()
                .setWord("Hello")
                .setNoChildren()
                .create();

        PhraseTree phrase = new PhraseTree("Hello","NN", "root");
        phrase.addChild(new PhraseTree("world", "NN", "nobj"));

        PhrasePattern.MatchResult matchResult = anychild.matchAgainst(phrase);
        assertTrue(matchResult.matches);

        PhrasePattern.MatchResult matchResult2 = noChild.matchAgainst(phrase);
        assertFalse(matchResult2.matches);


    }

}