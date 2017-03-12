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
        // How many times to repeat the pattern.
        var repeatMin: Int = 1, // At least this many
        var repeatMax: Int? = 1, // At most this many (0 = arbitrarily many)
        // List of child patterns.
        // If null, ignore any children.
        // If not null, children must match exactly.
        // Empty list means no children allowed.
        var children: List<PhrasePattern>?,
        var optional: Boolean = false) {

    class MatchResult(var matches: Boolean = false,
                      var matchScore: Double = 0.0,
                      var capturings: MutableMap<String, PhraseTree> = HashMap()) {
        fun merge(other: MatchResult) {
            this.matches = this.matches && other.matches
            this.matchScore += other.matchScore
            this.capturings.putAll(other.capturings)
        }
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

        val result = MatchResult(matches = false, matchScore = 0.0, capturings = mutableMapOf())

        // First, verify that the word, nature and role match the regexes
        if (word != null) {
            if (!word!!.matcher(phrase.rootWord).find()) {
                return result
            } else {
                // For each matching pattern,
                result.matchScore += 1.0;
            }
        }

        if (nature != null) {
            if (!nature!!.matcher(phrase.nature).find()) {
                return result
            } else {
                result.matchScore += 1.0;
            }
        }

        if (role != null) {
            if (!role!!.matcher(phrase.role).find()) {
                return result
            } else {
                result.matchScore += 1.0;
            }
        }

        // If information about children is present, match the children
        if (this.children != null) {
            val matchChildrenResult = matchChildren(phrase)

            if (!matchChildrenResult.matches) {
                return result
            } else {
                result.merge(matchChildrenResult)
            }
        }

        ////////////////////
        // -- Matches! -- //
        ////////////////////

        result.matches = true

        // Increase match score
        result.matchScore += 1.0

        // If necessary, capture
        if (name != null) {
            result.capturings[name as String] = phrase;
        }

        return result

    }

    // Match the children with each other
    private fun matchChildren(phrase: PhraseTree,
                              fromPhraseChildIndex: Int = 0,
                              fromPatternChildIndex: Int = 0): MatchResult {

        val phrases = phrase.children!!.drop(fromPatternChildIndex)
        val patterns = this.children!!.drop(fromPhraseChildIndex)

        val result = MatchResult(matches = false)

        if (patterns.isEmpty()) {
            result.matches = phrases.isEmpty()
            return result
        }

        val pattern = patterns.first()

        if (phrases.size < pattern.repeatMin) {
            result.matches = false
            return result
        }

        val firstMatches = phrases.take(pattern.repeatMin)
                                  .map {pattern.matchAgainst(it)}

        if (firstMatches.any {!it.matches}) {
            result.matches = false
            return result
        } else {
            firstMatches.forEach { result.merge(it) }
        }

        pattern.repeatMin.until(
                if (pattern.repeatMax == null) phrases.size
                else pattern.repeatMax!!.coerceAtMost(phrases.size))
            .map()

        
    }
}
