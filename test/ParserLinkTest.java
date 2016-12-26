import org.junit.Assert;
import org.junit.Test;

public class ParserLinkTest {

    @Test
    public void simpleTest1() {

        ParseTree parsed = SyntaxNetLink.parse("Let there be 15 cubes.");

        // Check whether the root word was parsed correctly
        Assert.assertEquals(parsed.getRootWord().getRootWord(), "Let");

        Assert.assertEquals(parsed.getRootWord().getRole(), "ROOT");

        Assert.assertEquals(parsed.getRootWord().getNature(), "VB");

    }

    @Test
    public void simpleTest2() {

        ParseTree parsed = SyntaxNetLink.parse("The quick brown fox jumps over the lazy dog.");

        // Check whether the root word was parsed correctly
        Assert.assertEquals(parsed.getRootWord().getRootWord(), "jumps");

        Assert.assertEquals(parsed.getRootWord().getRole(), "ROOT");

        Assert.assertEquals(parsed.getRootWord().getNature(), "VBZ");

    }

}