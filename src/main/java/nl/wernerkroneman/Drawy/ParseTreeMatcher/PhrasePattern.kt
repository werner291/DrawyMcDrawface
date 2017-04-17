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
                result.matchScore += 1.0
            }
        }

        if (nature != null) {
            if (!nature!!.matcher(phrase.nature).find()) {
                return result
            } else {
                result.matchScore += 1.0
            }
        }

        if (role != null) {
            if (!role!!.matcher(phrase.role).find()) {
                return result
            } else {
                result.matchScore += 1.0
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
            result.capturings[name as String] = phrase
        }

        return result

    }

    // Match the children with each other
    private fun matchChildren(phrase: PhraseTree): MatchResult {

        val phraseChildren = phrase.children
        val patternChildren = this.children!!

        // If no children are allowed, check if this is the case
        if (patternChildren.isEmpty()) {
            if (phraseChildren.isEmpty()) {
                return MatchResult(true, 1.0)
            } else {
                return MatchResult(false)
            }
        }

        /**
         * A 2-dimensional array of MatchResult objects.
         *
         * For some MatchResult r, resultTable[i][j] == r
         * means that with pattern children up to and excluding
         * this.children[j], r represents the best-possible way
         * to match the phrase children up to and excluding
         * phrase.children[i]
         */
        val resultTable = Array<Array<MatchResult?>>(phraseChildren.size + 1, {
            Array(patternChildren.size + 1, {
                null
            })
        })

        // No patterns and no phrases = match
        resultTable[0][0] = MatchResult(matches = true, matchScore = 0.0)

        // Phrases with no patterns = no match
        for (numPhrases in 1 .. phraseChildren.size) {
            resultTable[numPhrases][0] = MatchResult(matches = false)
        }

        // -------------------------- //
        // 0 patterns case dealt with //
        // -------------------------- //

        // Iterate over the various cells in the table
        // In both dimensions, in increasing order.
        // Skip numPatterns == 0 since it was dealt with previously.
        for (numPhrases in 0..phraseChildren.size) {
            for (numPatterns in 1..patternChildren.size) {
                // Find the contents of the cell
                resultTable[numPhrases][numPatterns] = computeOptimalResult(
                        phraseChildren.take(numPhrases),
                        patternChildren.take(numPatterns),
                        resultTable)
            }
        }

        return resultTable.last().last()!!
    }

    /**
     * Compute the best possible match, given phrases
     * up to and including phrases[numPhrases], and
     * patterns up to and including patterns[numPatterns]
     *
     * @pre numPatterns >= 1
     */
    fun computeOptimalResult(phrases: List<PhraseTree>,
                             patterns: List<PhrasePattern>,
                             resultTable: Array<Array<MatchResult?>>): MatchResult {

        // Reference to current pattern child, which is tha last pattern
        // in "patterns" truncated to x numPatterns
        var pattern = patterns.last()

        /*
         * Pattern "pattern" must match every phrase from a certain index
         * matchFrom, with matchFromAtLeast <= matchFrom <= matchFromAtMost,
         * with matchFrom chosen to be such that the total matchScore is
         * maximal and matchFrom is 0, inclusive or (numPatterns > 0 and
         * and resultTable[matchFrom][numPatterns-1].matches) is true.
         * (ie: previous pattern ik OK with basing from that cell)
         */
        val consumeAtLeast = pattern.repeatMin
        val consumeAtMost = if (pattern.repeatMax == null) phrases.size
        else (pattern.repeatMax!!).coerceAtMost(phrases.size)

        // Range of all possible match start positions
        return (consumeAtLeast..consumeAtMost)
                // Eliminate consumption numbers for which the previous patterns
                // would not find a match with the remaining phrases
                .filter({ consume -> resultTable[phrases.size - consume][patterns.size - 1]!!.matches })
                // For each remaining consumption number, match the phrases
                // to be consumed against the pattern
                .map { Pair(it, matchAllPhrases(pattern, phrases.subList(phrases.size - it, phrases.size))) }
                // Filter out non-matches
                .filter { it.second.matches }
                // Merge the match results with the match that was found earlier
                .map {
                    val result = MatchResult(it.second.matches,
                            it.second.matchScore,
                            HashMap(it.second.capturings))

                    result.merge(resultTable[phrases.size - it.first][patterns.size - 1]!!)

                    Pair(it.first, result)
                }
                // Get the maximum by match score
                .maxBy { it.second.matchScore }
                // Return the result, or return non-match result if null.
                ?.second ?: MatchResult(false)


    }

    /**
     * Match the pattern with all phrases in the list,
     * and return the combined MatchResult.
     *
     * If any doesn't match, the total MatchResult is false.
     */
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
