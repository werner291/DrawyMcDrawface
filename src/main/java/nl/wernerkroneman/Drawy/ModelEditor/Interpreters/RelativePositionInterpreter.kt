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

package nl.wernerkroneman.Drawy.ModelEditor.Interpreters;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel
import nl.wernerkroneman.Drawy.Modelling.Distance
import nl.wernerkroneman.Drawy.Modelling.Distance.Companion.ANY
import nl.wernerkroneman.Drawy.Modelling.Model
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree

class RelativePositionInterpreter(val relPos: RelativePositionConstraint.RelativePosition,
                                  val modelInterpreter: PatternInterpreter)
    : PatternInterpreter.InterpretedObjectFactory {

    override val interpretedTypePrediction: Class<*>
        get() = RelativePositionConstraint::class.java

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: MutableList<Any>): Any? {

        if (context.isEmpty()) {

            throw IllegalStateException("Relative position makes no sense without context.")

        } else if (context.last() is Model) {

            // TODO should I perhaps move this to the model interpreter?

            // Create the composite
            val composite = CompositeModel()

            // Put the context model  into the component
            val componentA = composite.addComponentForModel(context.last() as Model)

            // Replace the individual model by the composite in the context
            context.set(context.lastIndex, composite)

            // Interpret whatever this thing ig relative to
            val relativeTo = modelInterpreter.interpret(
                    capturings["relative_to"]!!,
                    Model::class,
                    context) as Model

            // Create a component for it.
            val componentB = composite.addComponentForModel(relativeTo)

            // ----------------------------
            // Create the actual constraint

            val distance = when (capturings["distance"]) {
                null -> Distance.ANY
                else -> modelInterpreter.interpret(
                        phrase = capturings["distance"]!!,
                            type = Distance::class,
                            context = context
                        ) ?: Distance.ANY
            } as Distance

            // Finally,add the constraint.
            val relativePositionConstraint = RelativePositionConstraint(
                    a = componentA, b = componentB, pos = relPos, dist = distance)



            composite.addConstraint(relativePositionConstraint)

            return relativePositionConstraint

        } else {
            throw UnsupportedOperationException("Unknown context: " + context)
        }

        return null

    }
}
