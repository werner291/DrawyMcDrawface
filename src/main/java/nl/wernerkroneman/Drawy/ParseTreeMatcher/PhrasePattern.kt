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
    private fun matchChildren(phrase: PhraseTree): MatchResult {

        val phraseChildren = phrase.children!!
        val patternChildren = this.children!!

        // If no children are allowed, check if this is the case
        if (patternChildren.isEmpty()) {
            if (phraseChildren.isEmpty()) {
                return MatchResult(true,1.0)
            } else {
                return MatchResult(false)
            }
        }

        /**
         * A 2-dimensional array of MatchResult objects.
         *
         * For some MatchResult r, resultTable[i][j] == r
         * means that with pattern children up to and including
         * this.children[j], r represents the best-possible way
         * to match the phrase children up to and including
         * phrase.children[i]
         */
        val resultTable = Array<Array<MatchResult>>(phraseChildren.size, {
            Array(patternChildren.size, {
                MatchResult(false,0.0,mutableMapOf())
            })
        })

        // Iterate over the various cells in the table
        // In both dimensions, in increasing order.
        for (phraseIndex in 0 until phraseChildren.size) {
            for (patternIndex in 0 until patternChildren.size) {

                resultTable[phraseIndex][patternIndex] = computeOptimalResult(phraseChildren,
                        phraseIndex,patternChildren,patternIndex,resultTable)
            }
        }

        return resultTable.last().last()
    }

    /**
     * Compute the best possible match, given phrases
     * up to and including phrases[phraseIndex], and
     * patterns up to and including patterns[patternIndex]
     */
    fun computeOptimalResult(phrases: List<PhraseTree>,
                             phraseIndex: Int,
                             patterns: List<PhrasePattern>,
                             patternIndex: Int,
                             resultTable: Array<Array<MatchResult>>) : MatchResult {

        // Reference to current pattern child
        var pattern = patterns[patternIndex]

        /*
         * Pattern "pattern" must match every phrase from a certain index
         * matchFrom, with matchFromAtLeast <= matchFrom <= matchFromAtMost,
         * with matchFrom chosen to be such that the total matchScore is
         * maximal and matchFrom is 0, inclusive or (patternIndex > 0 and
         * and resultTable[matchFrom][patternIndex-1].matches) is true.
         */

        if (patternIndex == 0) { // This is the first in the list of patterns
            if (pattern.repeatMax != null && pattern.repeatMax!! < phraseIndex+1) {
                // Cannot cover phrases[0]...phrases[patternIndex] by repeating
                return MatchResult(matches = false)
            } else if (pattern.repeatMin > phraseIndex+1) {
                // phrases[0]...phrases[patternIndex] not enough to cover repeat count
                return MatchResult(matches = false)
            } else {
                return matchAllPhrases(pattern, phrases.take(phraseIndex + 1))
            }

        } else { // There are previous patterns

            // Determine last possible start position
            val matchFromAtMost = phraseIndex - pattern.repeatMin+1
            // Determine first possible start position
            val matchFromAtLeast = if (pattern.repeatMax == null)
                0
            else
                (phraseIndex - pattern.repeatMax!! + 1).coerceAtLeast(0)

            // Range of all possible match start positions
            return (matchFromAtLeast..matchFromAtMost)
                    // Remove start positions that are not either the first prase
                    // or for which there exists a sub-problem match
                    // it-1 because we need to look back at a result of a previous phrase
                    .filter({it == 0 || resultTable[it-1][patternIndex-1].matches})
                    // Try to match with the fixed-length range
                    .map {Pair(it,matchAllPhrases(pattern, phrases.subList(it,phraseIndex+1)))}
                    // Filter out non-matches
                    .filter {it.second.matches}
                    // Merge the match results with the previous match
                    .map {
                        val result = MatchResult(it.second.matches,
                                                 it.second.matchScore,
                                                 HashMap(it.second.capturings))
                        if (it.first >= 1) {
                            result.merge(resultTable[it.first - 1][patternIndex - 1]!!)
                        }
                        Pair(it.first, result)}
                    // Get the maximum by match score
                    .maxBy { it.second.matchScore }
                    // Return the result, or return non-match result if null.
                    ?.second ?: MatchResult(false)

        }
    }

    fun matchAllPhrases(pattern: PhrasePattern, phrases: List<PhraseTree>): MatchResult {
        // Repeat count OK, all must match from 0, compute matches
        val matches = phrases.map({ pattern.matchAgainst(it) })

        if (matches.any { !it.matches }) {
            // All must match
            return MatchResult(matches = false)
        } else {
            // All match, accumulate match score and capturings
            val result = MatchResult(matches = true)
            for (match in matches) {
                result.matchScore += match.matchScore
                result.capturings.putAll(match.capturings)
            }
            return result
        }
    }
}
