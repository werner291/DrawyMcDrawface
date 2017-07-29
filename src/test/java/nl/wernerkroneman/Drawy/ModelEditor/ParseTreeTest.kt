/*
 * Copyright (c) 2017 Werner Kroneman
 *
 * This file is part of DrawyMcDrawface.
 *
 * DrawyMcDrawface is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DrawyMcDrawface is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DrawyMcDrawface.  If not, see <http://www.gnu.org/licenses/>.
 */

package nl.wernerkroneman.Drawy.ModelEditor

import org.junit.Assert
import org.junit.Test

class ParseTreeTest {

    @Test
    fun parseTest() {

        // Parse a tree
        val parserOutput = """
        |Let VB ROOT
        | +-- be VB ccomp
        |     +-- there EX expl
        |     +-- cubes NNS nsubj
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
    }

    @Test
    fun parseTestOnlyRoot() {

        val phrase = parsePhraseTree("Let VB ROOT")

        Assert.assertEquals("Let", phrase.rootWord)

        Assert.assertEquals("ROOT", phrase.role)

        Assert.assertEquals("VB", phrase.nature)

        Assert.assertEquals(0, phrase.children.size.toLong())

    }

}