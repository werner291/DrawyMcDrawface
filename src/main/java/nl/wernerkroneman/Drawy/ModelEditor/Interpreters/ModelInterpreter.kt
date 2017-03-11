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

import nl.wernerkroneman.Drawy.ModelEditor.Knowledge
import nl.wernerkroneman.Drawy.Modelling.Model
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import java.util.*

class ModelInterpreter(internal val knowledge: Knowledge,
                       internal val interpreter: PatternInterpreter) :
        PatternInterpreter.InterpretedObjectFactory {

    override val interpretedTypePrediction: Class<*>
        get() = Model::class.java

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: MutableList<Any>): Model {

        var phrase = capturings.get("name") ?:
                throw IllegalStateException("No name capturing")

        val simpleModel: Model = knowledge.getObject(phrase.rootWord) ?:
                throw NoSuchElementException("No known model named " + phrase.rootWord)

        // Add as current context
        context.add(simpleModel)

        for (child in phrase.children) {

            // Apply modifiers, if any
            interpreter.interpret(child, context = context)

        }

        // Pop the result off the context
        return context.removeAt(context.lastIndex) as Model

    }

}