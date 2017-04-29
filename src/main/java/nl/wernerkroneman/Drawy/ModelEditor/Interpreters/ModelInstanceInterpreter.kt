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

import com.cesarferreira.pluralize.singularize
import nl.wernerkroneman.Drawy.ModelEditor.Knowledge
import nl.wernerkroneman.Drawy.Modelling.ModelSpecification
import nl.wernerkroneman.Drawy.Modelling.RelativeLocation
import nl.wernerkroneman.Drawy.Modelling.Size
import nl.wernerkroneman.Drawy.Modelling.combineLocations
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InterpretationContext
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import java.util.*
import kotlin.reflect.KClass

class ModelInstanceInterpreter(internal val knowledge: Knowledge,
                               internal val interpreter: PatternInterpreter) :
        PatternInterpreter.InterpretedObjectFactory {

    class ModelInterpretationContext(val modelSoFar: ModelSpecification) : InterpretationContext

    override val interpretedTypePrediction: KClass<*>
        get() = ModelSpecification::class

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: List<InterpretationContext>): ModelSpecification {

        val phrase = capturings["name"] ?:
                throw IllegalStateException("No name capturing")

        val modelName = if (phrase.nature == "NN") phrase.rootWord
                        else phrase.rootWord.singularize()

        val result = knowledge.getObject(modelName)?.derive("a $modelName") ?:
                throw NoSuchElementException("No known model named " + phrase.rootWord)



        phrase.children
                .asSequence()
                .map { interpreter.interpret<Any?>(it, context = context + ModelInterpretationContext(result)) }
                .forEach {
                    when (it) {

                        is RelativeLocation -> {
                            result.location = combineLocations(result.location, it)
                        }

                    /*is RelativePositionConstraint -> {
                    when {
                        interpreted.b is CompositeModelSpecification.Component -> {
                            if (model !is CompositeModelSpecification) {
                                model = CompositeModelSpecification().apply {
                                    val comp = Component(model)
                                    directComponents.add(comp)
                                    comp
                                }
                            }
                            if (interpreted.a == null)
                                interpreted.a = model.directComponents.first()

                            model.constraints.add(interpreted)
                        }
                        interpreted.b is GroupModelSpecification.ComponentDesignator -> {
                            if (model !is GroupModelSpecification) {
                                throw IllegalStateException("Cannot use GroupModelSpecification placeholder without GroupModelSpecification context.")
                            }

                            val a = interpreted.a
                            val b = interpreted.b

                            if (a == null && b is GroupModelSpecification.ComponentDesignator.RelativeComponent) {
                                interpreted.a = if (b.offset < 0)
                                    GroupModelSpecification.ComponentDesignator.IndexRangeComponent(-b.offset, null)
                                else TODO()
                            }

                            model.constraints.add(interpreted)
                        }
                    }
                }*/

                        is Size -> {
                            result.size = it
                        }

                    }
                }

        return result

    }



}