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

/**
 * A structure containing a phrase tree pattern,
 * the class represented by that pattern,
 * and the factory function that produces the object.
 */
interface PhrasePatternInterpreter<T>/*Set<PhrasePattern> patterns;

    public T interpret(PhraseTree phrase) {

        for (PatternEntry entry : entries) {

            PhrasePattern.MatchResult matchResult = entry.pattern.matchAgainst(phrase);

            if (matchResult.matches) {
                return entry.value.apply(matchResult.capturings);
            }
        }

        return null;
    }*/