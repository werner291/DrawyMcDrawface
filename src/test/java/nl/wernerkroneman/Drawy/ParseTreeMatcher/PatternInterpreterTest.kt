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

import org.junit.Assert
import org.junit.Test

/**
 * Created by werner on 24-2-17.
 */
class PatternInterpreterTest {

    class Foo
    open class Bar
    class Baz : Bar()

    class FooInterpreter : PatternInterpreter.InterpretedObjectFactory {
        override val interpretedTypePrediction: Class<*>
            get() = Foo::class.java

        override fun interpret(capturings: Map<String, PhraseTree>,
                               context: MutableList<Any>) = Foo()
    }

    class BarInterpreter : PatternInterpreter.InterpretedObjectFactory {
        override val interpretedTypePrediction: Class<*>
            get() = Bar::class.java

        override fun interpret(capturings: Map<String, PhraseTree>,
                               context: MutableList<Any>) = Bar()
    }

    class BazInterpreter : PatternInterpreter.InterpretedObjectFactory {
        override val interpretedTypePrediction: Class<*>
            get() = Baz::class.java

        override fun interpret(capturings: Map<String, PhraseTree>,
                               context: MutableList<Any>) = Baz()
    }


    @Test
    @Throws(Exception::class)
    fun matchAgainst() {

        val terp = PatternInterpreter()

        terp.patterns.add(PatternInterpreter.InterpreterEntry(FooInterpreter(),
                PhrasePatternBuilder()
                        .setWord("Foo")
                        .create()))

        terp.patterns.add(PatternInterpreter.InterpreterEntry(BarInterpreter(),
                PhrasePatternBuilder()
                        .setWord("Bar")
                        .create()))

        terp.patterns.add(PatternInterpreter.InterpreterEntry(BazInterpreter(),
                PhrasePatternBuilder()
                        .setWord("Bar")
                        .addChild(PhrasePatternBuilder()
                                .setWord("Baz")
                                .create())
                        .create()))

        val testPhrase = PhraseTree("Bar", "TEST", "TEST")

        val result = terp.interpret(testPhrase, { true })
        Assert.assertTrue(result is Bar)
        Assert.assertFalse(result is Baz)

        testPhrase.addChild(PhraseTree("Troll", "TEST", "TEST"))

        val result2 = terp.interpret(testPhrase, { true })
        Assert.assertTrue(result is Bar)
        Assert.assertFalse(result is Baz)

        testPhrase.children.clear()
        testPhrase.addChild(PhraseTree("Baz", "TEST", "TEST"))

        val result3 = terp.interpret(testPhrase, { true })
        Assert.assertTrue(result3 is Bar)
        Assert.assertFalse(result3 is Baz)

    }
}