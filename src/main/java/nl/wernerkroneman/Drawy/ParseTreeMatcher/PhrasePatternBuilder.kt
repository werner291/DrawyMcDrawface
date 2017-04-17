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

import java.util.*
import java.util.regex.Pattern
import java.util.regex.Pattern.CASE_INSENSITIVE

/**
 * Convenient PhraseTreePatternNode builder facilitating
 * an internal DSL to specify a PhraseTree pattern.
 */
class PhrasePatternBuilder {
    private var word: Pattern? = null
    private var nature: Pattern? = null
    private var role: Pattern? = null
    private var name: String? = null
    private var repeatMin: Int = 1
    private var repeatMax: Int? = 1
    private var children: MutableList<PhrasePattern>? = null

    fun word(word: Pattern): PhrasePatternBuilder {
        this.word = word
        return this
    }

    fun nature(nature: Pattern): PhrasePatternBuilder {
        this.nature = nature
        return this
    }

    fun role(role: Pattern): PhrasePatternBuilder {
        this.role = role
        return this
    }


    fun word(word: String): PhrasePatternBuilder {
        this.word = Pattern.compile(word, CASE_INSENSITIVE)
        return this
    }

    fun nature(nature: String): PhrasePatternBuilder {
        this.nature = Pattern.compile(nature)
        return this
    }

    fun role(role: String): PhrasePatternBuilder {
        this.role = Pattern.compile(role)
        return this
    }

    fun name(name: String): PhrasePatternBuilder {
        this.name = name
        return this
    }

    fun children(children: MutableList<PhrasePattern>): PhrasePatternBuilder {
        this.children = children
        return this
    }

    fun children(vararg children: PhrasePattern): PhrasePatternBuilder {
        this.children = Arrays.asList(*children)
        return this
    }

    fun child(child: PhrasePattern): PhrasePatternBuilder {
        if (this.children == null) {
            this.children = ArrayList<PhrasePattern>()
        }
        this.children!!.add(child)
        return this
    }

    fun child(build: PhrasePatternBuilder.()->Unit): PhrasePatternBuilder {
        return child(buildPattern(build))
    }

    fun create(): PhrasePattern {
        return PhrasePattern(word, nature, role, name, repeatMin, repeatMax, children)
    }

    fun anyChildren(): PhrasePatternBuilder {
        this.children = null
        return this
    }

    fun noChildren(): PhrasePatternBuilder {

        if (this.children == null) {
            this.children = ArrayList<PhrasePattern>()
        }

        this.children!!.clear()

        return this
    }

    fun repeat(repeatMin: Int, repeatMax: Int? = repeatMin): PhrasePatternBuilder {
        this.repeatMin = repeatMin
        this.repeatMax = repeatMax
        return this
    }

    fun repeatAny(): PhrasePatternBuilder {
        repeatMin = 1
        repeatMax = null
        return this
    }

    fun optional(optional: Boolean = true): PhrasePatternBuilder {
        repeatMin = 0
        repeatMax = 1
        return this
    }
}

fun buildPattern(build: PhrasePatternBuilder.()->Unit) : PhrasePattern {
    val builder = PhrasePatternBuilder()
    builder.build()
    return builder.create()
}