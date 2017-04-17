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

import java.util.function.Predicate

/**
 * A datastructure representing a part of a sentence.
 * This part is characterised by:

 * - A root word with a corresponding grammatical nature and role
 * - A list of sub-parts.
 */
class PhraseTree(var rootWord: String,
                 var nature: String,
                 var role: String,
                 var children: MutableList<PhraseTree> = mutableListOf(),
                 var parent: PhraseTree? = null) {

    fun dfsFind(predicate: Predicate<PhraseTree>): PhraseTree? {

        if (predicate.test(this)) {
            return this
        }

        for (part in children) {
            val result = part.dfsFind(predicate)
            if (result != null)
                return result
        }

        return null
    }

    fun findFirstChild(predicate: Predicate<PhraseTree>): PhraseTree? {

        for (part in children) {
            val result = part.dfsFind(predicate)
            if (result != null)
                return result
        }

        return null
    }

    fun addChild(part: PhraseTree) {
        if (part.parent != null) {
            throw IllegalStateException("PhraseTree may not be a child to more than one parent.")
        }
        part.parent = this
        children.add(part)
    }

    fun deepCopy(): PhraseTree {
        val copy = PhraseTree(this.rootWord, this.nature, this.role)

        for (child in children) {
            copy.addChild(child.deepCopy())
        }

        return copy
    }

    override fun toString(): String {
        return "PhraseTree(rootWord='$rootWord', nature='$nature', role='$role', children=$children)"
    }
}
