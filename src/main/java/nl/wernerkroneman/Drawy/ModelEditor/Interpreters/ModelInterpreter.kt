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
import nl.wernerkroneman.Drawy.Modelling.*
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import java.util.*
import kotlin.reflect.KClass

class ModelInterpreter(internal val knowledge: Knowledge,
                       internal val interpreter: PatternInterpreter) :
        PatternInterpreter.InterpretedObjectFactory {

    override val interpretedTypePrediction: KClass<*>
        get() = Model::class

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: List<Any>): Model {

        var phrase = capturings.get("name") ?:
                throw IllegalStateException("No name capturing")

        val modelName = if (phrase.nature == "NN") phrase.rootWord
                        else phrase.rootWord.singularize()

        var model: Model = knowledge.getObject(modelName) ?:
                throw NoSuchElementException("No known model named " + phrase.rootWord)

        if (phrase.nature == "NNS") {
            model = GroupModel(1, model, phrase.rootWord)
        }

        for (child in phrase.children) {

            val interpreted = interpreter.interpret(child, context = context)

            when (interpreted) {

                is RelativePositionConstraint -> {
                    when {
                        interpreted.a == null &&
                                interpreted.b is CompositeModel.Component -> {
                            if (model !is CompositeModel) {
                                model = CompositeModel().apply { addComponentForModel(model) }
                            }
                            model.components.add(interpreted.b as CompositeModel.Component)
                            model.addConstraint(interpreted)
                        }
                        interpreted.b == GroupModel.Component.RECIPROCAL -> {
                            if (model !is GroupModel) {
                                throw IllegalStateException("Cannot use GroupModel placeholder without GroupModel context.")
                            }
                            interpreted.a = GroupModel.PLACEHOLDER_A
                            interpreted.b = GroupModel.PLACEHOLDER_B
                            model.constraints.add(interpreted)
                        }
                        interpreted.a == GroupModel.PLACEHOLDER_A &&
                                interpreted.b == GroupModel.PLACEHOLDER_B -> {
                            if (model !is GroupModel) {
                                throw IllegalStateException("Cannot use GroupModel placeholder without GroupModel context.")
                            }
                            model.constraints.add(interpreted)
                        }
                    }
                }

                is Int -> {
                    if (model !is GroupModel) {
                        model = GroupModel(interpreted, model, phrase.rootWord)
                    } else {
                        model.number = interpreted
                    }
                }

                is Modifier -> {
                    if (model !is VariantModel) {
                        model = VariantModel("", model)
                    }
                    model.modifiers.add(interpreted)
                }

            }

        }

        return model

    }

}