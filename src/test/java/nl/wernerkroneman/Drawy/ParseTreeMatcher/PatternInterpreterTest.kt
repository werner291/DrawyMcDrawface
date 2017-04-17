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

import nl.wernerkroneman.Drawy.ModelEditor.Interpreters.constantInterpreter
import org.junit.Assert
import org.junit.Test

/**
 * Created by werner on 24-2-17.
 */
class PatternInterpreterTest {

    class Foo
    open class Bar
    class Baz : Bar()

    @Test
    @Throws(Exception::class)
    fun matchAgainst() {

        val terp = PatternInterpreter()

        terp.patterns.add(PatternInterpreter.InterpreterEntry(constantInterpreter(Foo()),
                PhrasePatternBuilder()
                        .word("Foo")
                        .create()))

        terp.patterns.add(PatternInterpreter.InterpreterEntry(constantInterpreter(Bar()),
                PhrasePatternBuilder()
                        .word("Bar")
                        .create()))

        terp.patterns.add(PatternInterpreter.InterpreterEntry(constantInterpreter(Baz()),
                PhrasePatternBuilder()
                        .word("Bar")
                        .child(PhrasePatternBuilder()
                                .word("Baz")
                                .create())
                        .create()))

        val testPhrase = PhraseTree("Bar", "TEST", "TEST")

        val result = terp.interpret<Any?>(testPhrase)
        Assert.assertTrue(result is Bar)
        Assert.assertFalse(result is Baz)

        testPhrase.addChild(PhraseTree("Troll", "TEST", "TEST"))

        val result2 = terp.interpret<Any?>(testPhrase)
        Assert.assertTrue(result is Bar)
        Assert.assertFalse(result is Baz)

        testPhrase.children.clear()
        testPhrase.addChild(PhraseTree("Baz", "TEST", "TEST"))

        val result3 = terp.interpret<Any?>(testPhrase)
        Assert.assertTrue(result3 is Bar)
        Assert.assertTrue(result3 is Baz)

    }
}