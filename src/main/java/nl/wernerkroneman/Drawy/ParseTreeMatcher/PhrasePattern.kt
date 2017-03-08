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
 * Represents a node in a phrase tree pattern,
 * and the root of a sub-tree.
 */
class PhrasePattern(// Regexes for the word, nature and role
        // Null means any will do.
        var word: Pattern?,
        var nature: Pattern?,
        var role: Pattern?,
        // If not null, MatchResults capturings will include reference
        // to phrase tree under this key name
        var name: String?,
        var repeat: Int?,
        // List of child patterns.
        // If null, ignore any children.
        // If not null, children must match exactly.
        // Empty list means no children allowed.
        var children: List<PhrasePattern>?) {

    class MatchResult {
        var matches: Boolean = false
        internal var matchScore = 0.0
        var capturings: MutableMap<String, PhraseTree> = HashMap()
    }

    /**
     * Match this pattern against the provided PhraseTree
     *
     * @param phrase The phrase tree against which to match.
     *
     * @return A MatchResult. {@code matches} will be true if the pattern matches,
     *              {@code matchScore} indicates how close it is (only meaningful if
     *              {@code matches == true}, and {@code capturings} is a map of captures.
     */
    fun matchAgainst(phrase: PhraseTree): MatchResult {
        val matchResult = MatchResult()

        matchResult.matches = this.matchAgainstImpl(phrase, matchResult)

        return matchResult
    }

    protected fun matchAgainstImpl(phrase: PhraseTree, result: MatchResult): Boolean {

        // First, verify that the word, nature and role match the regexes
        if (word != null) {
            if (!word!!.matcher(phrase.rootWord).find()) {
                return false
            } else {
                // For each matching pattern,
                result.matchScore += 1.0;
            }
        }

        if (nature != null) {
            if (!nature!!.matcher(phrase.nature).find()) {
                return false
            } else {
                result.matchScore += 1.0;
            }
        }

        if (role != null) {
            if (!role!!.matcher(phrase.role).find()) {
                return false
            } else {
                result.matchScore += 1.0;
            }
        }

        // If information about children is present, match the children
        if (this.children != null && !matchChildren(phrase, result)) {
            return false
        }

        ////////////////////
        // -- Matches! -- //
        ////////////////////

        // Increase match score
        result.matchScore += 1.0

        // If necessary, capture
        if (name != null) {
            result.capturings[name as String] = phrase;
        }

        return true

    }

    // Match the children with each other
    private fun matchChildren(phrase: PhraseTree, result: MatchResult): Boolean {
        // Get an iterator over the children of the phrase
        val phraseItr = phrase.children.listIterator()

        // If no children are allowed, but it has children, fail test
        if (this.children!!.isEmpty() && !phrase.children.isEmpty()) {
            return false
        }

        // Try to consume all children with the child patterns
        for (pattern in this.children!!) {

            // Repeat for required number of times
            for (copies in pattern.repeat!! downTo 1) {

                if (!phraseItr.hasNext()) {
                    // There should be a child, but there isn't.
                    return false
                } else {
                    val phraseChild = phraseItr.next()

                    // Recursive matching
                    if (!pattern.matchAgainstImpl(phraseChild, result)) {
                        return false
                    }
                }

            }

        }

        // All children should have been consumed.
        return !phraseItr.hasNext()
    }
}
