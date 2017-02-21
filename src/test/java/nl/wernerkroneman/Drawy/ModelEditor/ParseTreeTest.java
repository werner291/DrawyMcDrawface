package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.ModelEditor.ParseTree;
import org.junit.Assert;
import org.junit.Test;

public class ParseTreeTest {

    @Test
    public void parseTest() {

        // Parse a tree
        String parserOutput = "Let VB ROOT\n"
                + " +-- be VB ccomp\n"
                + "     +-- there EX expl\n"
                + "     +-- cubes NNS nsubj\n"
                + "         +-- 15 CD num";

        ParseTree tree = new ParseTree(
                parserOutput);

        // Check whether the root word was parsed correctly
        Assert.assertEquals("Let", tree.getRootWord().getRootWord());

        Assert.assertEquals("ROOT", tree.getRootWord().getRole());

        Assert.assertEquals("VB", tree.getRootWord().getNature());

        // Randomly check tree structure
        Assert.assertEquals(1, tree.getRootWord().getChildren().size());

        Assert.assertEquals(2, tree.getRootWord().getChildren().get(0).getChildren().size());

        // Check correct parsing of one of the children
        Assert.assertEquals("cubes", tree.getRootWord().getChildren().get(0).getChildren().get(1).getRootWord());

        Assert.assertEquals("nsubj", tree.getRootWord().getChildren().get(0).getChildren().get(1).getRole());

        Assert.assertEquals("NNS", tree.getRootWord().getChildren().get(0).getChildren().get(1).getNature());

        Assert.assertEquals(parserOutput, tree.toString());
    }

    @Test
    public void parseTestOnlyRoot() {

        ParseTree tree = new ParseTree("Let VB ROOT");

        Assert.assertEquals("Let", tree.getRootWord().getRootWord());

        Assert.assertEquals("ROOT", tree.getRootWord().getRole());

        Assert.assertEquals("VB", tree.getRootWord().getNature());

        Assert.assertEquals(0, tree.getRootWord().getChildren().size());

    }

}