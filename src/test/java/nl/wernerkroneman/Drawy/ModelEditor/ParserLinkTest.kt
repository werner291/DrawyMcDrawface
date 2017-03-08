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

class ParserLinkTest {

    @Test
    fun simpleTest1() {

        val parsed = SyntaxNetLink.parse("Let there be 15 cubes.")

        // Check whether the root word was parsed correctly
        Assert.assertEquals(parsed.rootWord, "Let")

        Assert.assertEquals(parsed.role, "ROOT")

        Assert.assertEquals(parsed.nature, "VB")

    }

    @Test
    fun simpleTest2() {

        val parsed = SyntaxNetLink.parse("The quick brown fox jumps over the lazy dog.")

        // Check whether the root word was parsed correctly
        Assert.assertEquals(parsed.rootWord, "jumps")

        Assert.assertEquals(parsed.role, "ROOT")

        Assert.assertEquals(parsed.nature, "VBZ")

    }

}