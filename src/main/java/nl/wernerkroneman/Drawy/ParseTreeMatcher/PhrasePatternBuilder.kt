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

/**
 * Convenient PhraseTreePatternNode builder facilitating
 * an internal DSL to specify a PhraseTree pattern.
 */
class PhrasePatternBuilder {
    private var word: Pattern? = null
    private var nature: Pattern? = null
    private var role: Pattern? = null
    private var name: String? = null
    private var repeat: Int? = 1
    private var children: MutableList<PhrasePattern>? = null

    fun setWord(word: Pattern): PhrasePatternBuilder {
        this.word = word
        return this
    }

    fun setNature(nature: Pattern): PhrasePatternBuilder {
        this.nature = nature
        return this
    }

    fun setRole(role: Pattern): PhrasePatternBuilder {
        this.role = role
        return this
    }


    fun setWord(word: String): PhrasePatternBuilder {
        this.word = Pattern.compile(word)
        return this
    }

    fun setNature(nature: String): PhrasePatternBuilder {
        this.nature = Pattern.compile(nature)
        return this
    }

    fun setRole(role: String): PhrasePatternBuilder {
        this.role = Pattern.compile(role)
        return this
    }

    fun setName(name: String): PhrasePatternBuilder {
        this.name = name
        return this
    }

    fun setRepeat(repeat: Int): PhrasePatternBuilder {
        this.repeat = repeat
        return this
    }

    fun setChildren(children: MutableList<PhrasePattern>): PhrasePatternBuilder {
        this.children = children
        return this
    }

    fun setChildren(vararg children: PhrasePattern): PhrasePatternBuilder {
        this.children = Arrays.asList(*children)
        return this
    }

    fun addChild(child: PhrasePattern): PhrasePatternBuilder {
        if (this.children == null) {
            this.children = ArrayList<PhrasePattern>()
        }
        this.children!!.add(child)
        return this
    }

    fun create(): PhrasePattern {
        return PhrasePattern(word, nature, role, name, repeat, children)
    }

    fun setAnyChildren(): PhrasePatternBuilder {
        this.children = null
        return this
    }

    fun setNoChildren(): PhrasePatternBuilder {

        if (this.children == null) {
            this.children = ArrayList<PhrasePattern>()
        }

        this.children!!.clear()

        return this
    }

    fun setRepeatAny(): PhrasePatternBuilder {
        repeat = null;
        return this;
    }
}