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
import nl.wernerkroneman.Drawy.ModelEditor.InterpretationContext
import nl.wernerkroneman.Drawy.ModelEditor.Knowledge
import nl.wernerkroneman.Drawy.Modelling.Model
import nl.wernerkroneman.Drawy.Modelling.RelativeLocation
import nl.wernerkroneman.Drawy.Modelling.SizeModifier
import nl.wernerkroneman.Drawy.Modelling.combineLocations
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import java.util.*
import kotlin.reflect.KClass

class ModelInstanceInterpreter(internal val knowledge: Knowledge,
                               internal val interpreter: PatternInterpreter) :
        PatternInterpreter.InterpretedObjectFactory {

    override val interpretedTypePrediction: KClass<*>
        get() = Model::class

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: List<InterpretationContext>): Model {

        var phrase = capturings.get("name") ?:
                throw IllegalStateException("No name capturing")

        val modelName = if (phrase.nature == "NN") phrase.rootWord
                        else phrase.rootWord.singularize()

        var result = knowledge.getObject(modelName)?.derive("a $modelName") ?:
                throw NoSuchElementException("No known model named " + phrase.rootWord)


        for (child in phrase.children) {

            val interpreted = interpreter.interpret<Any?>(child, context = context)

            when (interpreted) {

                is RelativeLocation -> {
                    result.location = combineLocations(result.location, interpreted)
                }

            /*is RelativePositionConstraint -> {
                when {
                    interpreted.b is CompositeModel.Component -> {
                        if (model !is CompositeModel) {
                            model = CompositeModel().apply {
                                val comp = Component(model)
                                components.add(comp)
                                comp
                            }
                        }
                        if (interpreted.a == null)
                            interpreted.a = model.components.first()

                        model.constraints.add(interpreted)
                    }
                    interpreted.b is GroupModel.ComponentDesignator -> {
                        if (model !is GroupModel) {
                            throw IllegalStateException("Cannot use GroupModel placeholder without GroupModel context.")
                        }

                        val a = interpreted.a
                        val b = interpreted.b

                        if (a == null && b is GroupModel.ComponentDesignator.RelativeComponent) {
                            interpreted.a = if (b.offset < 0)
                                GroupModel.ComponentDesignator.IndexRangeComponent(-b.offset, null)
                            else TODO()
                        }

                        model.constraints.add(interpreted)
                    }
                }
            }*/

                is SizeModifier -> {
                    result.size = interpreted
                }

            }

        }

        return result

    }



}