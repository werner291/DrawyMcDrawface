import org.junit.Assert;
import org.junit.Test;

public class ParseTreeTest {

    @Test
    public void parseTest() {

        // Parse a tree
        ParseTree tree = new ParseTree(
                  "Let VB ROOT\n"
                + " +-- be VB ccomp\n"
                + "     +-- there EX expl\n"
                + "     +-- cubes NNS nsubj\n"
                + "         +-- 15 CD num");

        // Check whether the root word was parsed correctly
        Assert.assertEquals("Let", tree.getRootWord().getRootWord());

        Assert.assertEquals("ROOT", tree.getRootWord().getRole());

        Assert.assertEquals("VB", tree.getRootWord().getNature());

        // Randomly check tree structure
        Assert.assertEquals(1, tree.getRootWord().children.size());

        Assert.assertEquals(2, tree.getRootWord().children.get(0).children.size());

        // Check correct parsing of one of the children
        Assert.assertEquals("cubes", tree.getRootWord().children.get(0).children.get(1).getRootWord());

        Assert.assertEquals("nsubj", tree.getRootWord().children.get(0).children.get(1).getRole());

        Assert.assertEquals("NNS", tree.getRootWord().children.get(0).children.get(1).getNature());


    }

    @Test
    public void parseTestOnlyRoot() {

        ParseTree tree = new ParseTree("Let VB ROOT");

        Assert.assertEquals("Let", tree.getRootWord().getRootWord());

        Assert.assertEquals("ROOT", tree.getRootWord().getRole());

        Assert.assertEquals("VB", tree.getRootWord().getNature());

        Assert.assertEquals(0, tree.getRootWord().children.size());

    }

}