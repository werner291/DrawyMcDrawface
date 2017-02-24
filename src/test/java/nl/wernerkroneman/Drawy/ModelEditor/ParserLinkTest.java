package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree;
import org.junit.Assert;
import org.junit.Test;

public class ParserLinkTest {

    @Test
    public void simpleTest1() {

        PhraseTree parsed = SyntaxNetLink.INSTANCE.parse("Let there be 15 cubes.");

        // Check whether the root word was parsed correctly
        Assert.assertEquals(parsed.getRootWord(), "Let");

        Assert.assertEquals(parsed.getRole(), "ROOT");

        Assert.assertEquals(parsed.getNature(), "VB");

    }

    @Test
    public void simpleTest2() {

        PhraseTree parsed = SyntaxNetLink.INSTANCE.parse("The quick brown fox jumps over the lazy dog.");

        // Check whether the root word was parsed correctly
        Assert.assertEquals(parsed.getRootWord(), "jumps");

        Assert.assertEquals(parsed.getRole(), "ROOT");

        Assert.assertEquals(parsed.getNature(), "VBZ");

    }

}