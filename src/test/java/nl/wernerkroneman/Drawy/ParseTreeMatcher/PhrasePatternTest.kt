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

package nl.wernerkroneman.Drawy.ParseTreeMatcher

import org.junit.Assert.*
import org.junit.Test

/**
 * Created by werner on 17-2-17.
 */
class PhrasePatternTest {

    @Test
    @Throws(Exception::class)
    fun matchAgainst() {

        val pattern = PhrasePatternBuilder()
                .word("Hello")
                .noChildren()
                .create()

        val phrase = PhraseTree("Hello", "NN", "root")

        assertTrue(pattern.matchAgainst(phrase).matches)

        phrase.addChild(PhraseTree("world", "NN", "nobj"))

        assertFalse(pattern.matchAgainst(phrase).matches)

    }

    @Test
    @Throws(Exception::class)
    fun matchAgainstPatternWithChild() {

        val pattern = PhrasePatternBuilder()
                .word("Hello")
                .children(
                        PhrasePatternBuilder()
                                .word("world")
                                .create()
                ).create()

        val phrase = PhraseTree("Hello", "NN", "root")
        phrase.addChild(PhraseTree("world", "NN", "nobj"))

        assertTrue(pattern.matchAgainst(phrase).matches)
    }

    @Test
    @Throws(Exception::class)
    fun matchAgainstPatternWithChildAndDependency() {

        val pattern = PhrasePatternBuilder()
                .word("Hello")
                .children(
                        PhrasePatternBuilder()
                                .word("world")
                                .create(),
                        PhrasePatternBuilder()
                                .name("testdependency")
                                .create()
                ).create()

        val phrase = PhraseTree("Hello", "NN", "root")
        phrase.addChild(PhraseTree("world", "NN", "nobj"))
        phrase.addChild(PhraseTree("bar", "NN", "nobj"))

        val matchResult = pattern.matchAgainst(phrase)
        assertTrue(matchResult.matches)

        assertEquals(1, matchResult.capturings.size.toLong())
        assertEquals("bar", matchResult.capturings["testdependency"]!!.rootWord)
    }

    @Test
    @Throws(Exception::class)
    fun matchAgainstPatternAnychildNochild() {

        val anychild = PhrasePatternBuilder()
                .word("Hello")
                .anyChildren()
                .create()

        val noChild = PhrasePatternBuilder()
                .word("Hello")
                .noChildren()
                .create()

        val phrase = PhraseTree("Hello", "NN", "root")
        phrase.addChild(PhraseTree("world", "NN", "nobj"))

        val matchResult = anychild.matchAgainst(phrase)
        assertTrue(matchResult.matches)

        val matchResult2 = noChild.matchAgainst(phrase)
        assertFalse(matchResult2.matches)
    }

    @Test
    fun matchWithDeterministicNonrepeat() {
        val pattern = buildPattern {
            child( buildPattern { word("a") })
            child( buildPattern { word("b") })
            child( buildPattern { word("c") })
            child( buildPattern { word("d") })
            child( buildPattern { word("e") })
            child( buildPattern { word("f") })
        }

        val phrase = PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("b","test","test"),
                PhraseTree("c","test","test"),
                PhraseTree("d","test","test"),
                PhraseTree("e","test","test"),
                PhraseTree("f","test","test")
        ))

        assertTrue(pattern.matchAgainst(phrase).matches)
    }

    @Test
    fun matchWithDeterministicRepeat() {
        val pattern = buildPattern {
            child {
                word("a")
                repeat(6)
            }
        }

        val phrase = PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test")
        ))

        assertTrue(pattern.matchAgainst(phrase).matches)

        val phraseNoRepeat = PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("b","test","test"),
                PhraseTree("c","test","test"),
                PhraseTree("d","test","test"),
                PhraseTree("e","test","test"),
                PhraseTree("f","test","test")
        ))

        assertFalse(pattern.matchAgainst(phraseNoRepeat).matches)
    }

    @Test
    fun matchWithNondeterministicRepeat() {
        val pattern = buildPattern {
            child( buildPattern {
                word("a")
                repeat(2,5)
            })
        }

        assertFalse(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test")
        ))).matches)

        assertTrue(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test")
        ))).matches)

        assertTrue(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test")
        ))).matches)

        assertTrue(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test")
        ))).matches)

        assertFalse(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test")
        ))).matches)
    }

    @Test
    fun matchWithNondeterministicRepeatMultiple() {
        val pattern = buildPattern {
            child {
                word("a")
                repeat(1,2)
            }
            child {
                word("b")
                optional()
            }
        }

        assertTrue(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test")
        ))).matches)

        assertTrue(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test")
        ))).matches)

        assertFalse(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test"),
                PhraseTree("a","test","test")
        ))).matches)

        assertFalse(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("b","test","test")
        ))).matches)

        assertTrue(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("a","test","test"),
                PhraseTree("b","test","test")
        ))).matches)

    }

    @Test
    fun matchWithNonMatchingFirstNondeterministicSecond() {
        val pattern = buildPattern {
            child {
                word("a")
                repeat(1)
            }
            child {
                repeat(1,2)
            }
        }

        assertFalse(pattern.matchAgainst(PhraseTree("root","ROOT","root", mutableListOf(
                PhraseTree("b","test","test"),
                PhraseTree("b","test","test")
        ))).matches)

    }

}