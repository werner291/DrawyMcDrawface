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

import nl.wernerkroneman.Drawy.ModelEditor.InterpretationContext
import nl.wernerkroneman.Drawy.ModelEditor.SceneComponent
import nl.wernerkroneman.Drawy.ModelEditor.SceneComponentRelation
import nl.wernerkroneman.Drawy.Modelling.Distance
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import kotlin.reflect.KClass

class RelativePositionInterpreter(val relPos: RelativePositionConstraint.RelativePosition,
                                  val modelInterpreter: PatternInterpreter)
    : PatternInterpreter.InterpretedObjectFactory {

    override val interpretedTypePrediction: KClass<*>
        get() = SceneComponentRelation::class

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: List<InterpretationContext>): SceneComponentRelation =
            SceneComponentRelation(
                    right = modelInterpreter.interpret(
                            type = SceneComponent::class,
                            phrase = capturings["relative_to"]!!,
                            context = context) as SceneComponent,
                    relPos = relPos,
                    dist = when (capturings["distance"]) {
                        null -> Distance.ANY
                        else -> modelInterpreter.interpret(
                                phrase = capturings["distance"]!!,
                                type = Distance::class,
                                context = context
                        ) ?: Distance.ANY
                    } as Distance)
}
