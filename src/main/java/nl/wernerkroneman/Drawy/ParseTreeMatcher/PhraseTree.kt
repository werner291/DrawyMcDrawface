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
class PhraseTree(val rootWord: String,
                 val nature: String,
                 val role: String,
                 val children: List<PhraseTree> = emptyList()) {

    fun dfsFind(predicate: Predicate<PhraseTree>): PhraseTree? {

        if (predicate.test(this)) {
            return this
        }

        return children
                .asSequence()
                .map { it.dfsFind(predicate) }
                .firstOrNull { it != null }
    }

    fun findFirstChild(predicate: Predicate<PhraseTree>): PhraseTree? {

        return children
                .asSequence()
                .map { it.dfsFind(predicate) }
                .firstOrNull { it != null }
    }

    override fun toString(): String {
        return "PhraseTree(rootWord='$rootWord', nature='$nature', role='$role', children=$children)"
    }
}
