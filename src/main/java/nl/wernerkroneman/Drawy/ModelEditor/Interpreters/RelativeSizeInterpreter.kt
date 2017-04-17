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

package nl.wernerkroneman.Drawy.ModelEditor.Interpreters

import nl.wernerkroneman.Drawy.Modelling.Scalar
import nl.wernerkroneman.Drawy.Modelling.Size
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InterpretationContext
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InvalidContextException
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import kotlin.reflect.KClass

class RelativeSizeInterpreter(val ratio: Scalar) : PatternInterpreter.InterpretedObjectFactory {

    override val interpretedTypePrediction: KClass<*>
        get() = Size::class

    override fun interpret(capturings: Map<String, PhraseTree>, context: List<InterpretationContext>): Any? {
        val modelContext = context.findLast { it is ModelInstanceInterpreter.ModelInterpretationContext }
                as? ModelInstanceInterpreter.ModelInterpretationContext ?:
                throw InvalidContextException("Context must contain ModelInterpretationContext")

        return modelContext.modelSoFar.size * ratio
    }

}