package nl.wernerkroneman.Drawy.ParseTreeMatcher;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by werner on 17-2-17.
 */
public class MainMatcherTest {

    static class Foo {}
    static class Bar {}
    static class Baz extends Bar {}

    @Test
    public void matchSimple() throws Exception {

        MainMatcher matcher = new MainMatcher();

        matcher.addPattern(new PhrasePattern(Foo.class,
                new PhraseTreeNodeBuilder()
                        .setWord("Alice")
                        .createPhraseTreeNode(),
                (mr) -> new Foo()));

        matcher.addPattern(new PhrasePattern(Bar.class,
                new PhraseTreeNodeBuilder()
                        .setWord("Bob")
                        .createPhraseTreeNode(),
                (mr) -> new Bar()));

        matcher.addPattern(new PhrasePattern(Baz.class,
                new PhraseTreeNodeBuilder()
                        .setWord("Bob")
                        .setChildren(
                            new PhraseTreeNodeBuilder()
                                .setWord("Junior")
                                .createPhraseTreeNode()
                        ).createPhraseTreeNode(),
                (mr) -> new Baz()));

        // ----------------------------------

        PhraseTree tree = new PhraseTree("Alice", "NN", "nsubj");

        Object match = matcher.match(tree);

        assertTrue(match instanceof Foo);

        // ----------------------------------

        PhraseTree tree2 = new PhraseTree("Bob", "NN", "nsubj");

        Object match2 = matcher.match(tree2);

        assertTrue(match2 instanceof Bar);

        // ----------------------------------

        PhraseTree tree3 = new PhraseTree("Bob", "NN", "nsubj");
        tree3.addChild(new PhraseTree("Junior", "NN", "test"));

        Object match3 = matcher.match(tree3);

        assertTrue(match3 instanceof Bar);
        assertTrue(match3 instanceof Baz);

    }

}