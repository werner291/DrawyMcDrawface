package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.ModelEditor.*
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import org.junit.Assert
import org.junit.Test

class ParseTreeTest {

    @Test
    fun parseTest() {

        // Parse a tree
        val parserOutput = """
        |Let VB ROOT\n"
        | +-- be VB ccomp\n"
        |     +-- there EX expl\n"
        |     +-- cubes NNS nsubj\n"
        |         +-- 15 CD num
        """.trimMargin()

        val phrase = parsePhraseTree(parserOutput)

        // Check whether the root word was parsed correctly
        Assert.assertEquals("Let", phrase.rootWord)

        Assert.assertEquals("ROOT", phrase.role)

        Assert.assertEquals("VB", phrase.nature)

        // Randomly check tree structure
        Assert.assertEquals(1, phrase.children.size.toLong())

        Assert.assertEquals(2, phrase.children[0].children.size.toLong())

        // Check correct parsing of one of the children
        Assert.assertEquals("cubes", phrase.children[0].children[1].rootWord)

        Assert.assertEquals("nsubj", phrase.children[0].children[1].role)

        Assert.assertEquals("NNS", phrase.children[0].children[1].nature)

        Assert.assertEquals(parserOutput, phrase.toString())
    }

    @Test
    fun parseTestOnlyRoot() {

        val phrase = parsePhraseTree("Let VB ROOT")

        Assert.assertEquals("Let", phrase.getRootWord())

        Assert.assertEquals("ROOT", phrase.getRole())

        Assert.assertEquals("VB", phrase.getNature())

        Assert.assertEquals(0, phrase.getChildren().size.toLong())

    }

}