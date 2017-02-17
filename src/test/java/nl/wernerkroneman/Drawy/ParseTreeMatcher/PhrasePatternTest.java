package nl.wernerkroneman.Drawy.ParseTreeMatcher;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by werner on 17-2-17.
 */
public class PhrasePatternTest {

    @Test
    public void matchAgainst() throws Exception {

        PhrasePattern pattern = new PhrasePattern(Object.class,
                new PhraseTreeNodeBuilder()
                        .setWord("Hello")
                        .createPhraseTreeNode(),
                (a)->null);

        PhraseTree phrase = new PhraseTree("Hello","NN", "root");

        assertTrue(pattern.matchAgainst(phrase).matches);

        phrase.addChild(new PhraseTree("world", "NN", "nobj"));

        assertFalse(pattern.matchAgainst(phrase).matches);

    }

    @Test
    public void matchAgainstPatternWithChild() throws Exception {

        PhrasePattern pattern = new PhrasePattern(Object.class,
                new PhraseTreeNodeBuilder()
                        .setWord("Hello")
                        .setChildren(
                                new PhraseTreeNodeBuilder()
                                        .setWord("world")
                                        .createPhraseTreeNode()
                        ).createPhraseTreeNode(),
                (a)->null);

        PhraseTree phrase = new PhraseTree("Hello","NN", "root");
        phrase.addChild(new PhraseTree("world", "NN", "nobj"));

        assertTrue(pattern.matchAgainst(phrase).matches);
    }
    
    static class Foo {}

    @Test
    public void matchAgainstPatternWithChildAndDependency() throws Exception {

        PhrasePattern pattern = new PhrasePattern(Object.class,
                new PhraseTreeNodeBuilder()
                        .setWord("Hello")
                        .setChildren(
                                new PhraseTreeNodeBuilder()
                                        .setWord("world")
                                        .createPhraseTreeNode(),
                                new PhraseTreeNodeBuilder()
                                        .setRequiredSubpatternType(Foo.class)
                                        .setName("testdependency")
                                        .createPhraseTreeNode()
                        ).createPhraseTreeNode(),
                (a)->null);

        PhraseTree phrase = new PhraseTree("Hello","NN", "root");
        phrase.addChild(new PhraseTree("world", "NN", "nobj"));
        phrase.addChild(new PhraseTree("bar", "NN", "nobj"));

        PhrasePattern.MatchResult matchResult = pattern.matchAgainst(phrase);
        assertTrue(matchResult.matches);

        assertEquals(1,matchResult.capturings.size());
        assertEquals(1,matchResult.dependencies.size());
        assertEquals("bar",matchResult.capturings.get("testdependency").getRootWord());
    }

}