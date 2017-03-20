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
import kotlin.reflect.KClass
import kotlin.reflect.KType
import kotlin.reflect.full.isSuperclassOf

/**
 * Framework interpreter that allows one to map
 * {@link PhrasePattern}s to functions that return objects.
 */
open class PatternInterpreter {
    internal var patterns: MutableCollection<PatternInterpreter.InterpreterEntry> = ArrayList()

    /**
     * Attempt to interpret the phrase according to the interpretation rules.
     *
     * Rule that matches and has highest matching score is used.
     *
     * @param phrase
     * *
     * @param filter
     *
     * @param context Mutable list representing the context.
     *                Rule: Resulting list must always be of the same size.
     *
     * @return
     */
    fun interpret(phrase: PhraseTree,
                  type: KClass<*> = Any::class,
                  context: List<Any> = mutableListOf()): Any? {

        for (entry in patterns) {
            if (type.isSuperclassOf(entry.objectFactory.interpretedTypePrediction)) {
                val result = entry.pattern.matchAgainst(phrase)

                if (result.matches) {
                    val interpretation = entry.objectFactory.interpret(result.capturings, context)
                    return interpretation
                }
            }
        }
        return null
    }

    /**
     * An interface for classes that take the capturings
     * from a [PhrasePattern.MatchResult] and returns
     * the interpretation of it.
     *
     * It may return null if the interpretation fails.
     *
     * @param The type of object that results from interpretation.
     */
    interface InterpretedObjectFactory {

        val interpretedTypePrediction: KClass<*>

        /**
         * Process the matchings of a certain phrase.
         *
         * @param capturings A map containing the captured phrase tree pieces
         * @param context A list representing the context, IE a list of all
         *                higher-level interpreted objects.
         *
         * @return An interpreted object corresponding to the capturings, if any
         *
         * @modify {@code context} in case this phrase capturing reveals
         *         more information about a parent context.
         */
        fun interpret(capturings: Map<String, PhraseTree>,
                      context: List<Any>): Any?
    }

    /**
     * Represents an interpretation rule.
     * It consists of a [PhrasePattern] that specifies
     * on which kind of phrases it will apply, and an
     * [InterpretedObjectFactory] that serves to get the
     * final object.
     */
    class InterpreterEntry(internal var objectFactory: PatternInterpreter.InterpretedObjectFactory,
                           internal var pattern: PhrasePattern)

    fun addPattern(factory: InterpretedObjectFactory,
                   pattern: PhrasePattern) {
        patterns.add(InterpreterEntry(factory,
                pattern))
    }
}