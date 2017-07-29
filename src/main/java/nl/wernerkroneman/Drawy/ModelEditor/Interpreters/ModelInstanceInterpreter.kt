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
import nl.wernerkroneman.Drawy.Modelling.*
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InterpretationContext
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import java.awt.Color
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

        val result = knowledge.getObject(modelName).derive("a $modelName")



        phrase.children
                .asSequence()
                .map { interpreter.interpret<Any?>(it, context = context + ModelInterpretationContext(result)) }
                .forEach {
                    when (it) {

                        is RelativeLocation -> {
                            result.location = combineLocations(result.location, it)
                        }

                        is Color -> {
                            result.color = it
                        }

                        is Size -> {
                            result.size = it
                        }

                    }
                }

        return result

    }



}