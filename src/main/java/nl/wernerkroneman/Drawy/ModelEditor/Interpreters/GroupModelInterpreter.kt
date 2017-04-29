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
import nl.wernerkroneman.Drawy.Modelling.GroupModelSpecification
import nl.wernerkroneman.Drawy.Modelling.GroupModelSpecificationBase
import nl.wernerkroneman.Drawy.Modelling.ModelSpecification
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InterpretationContext
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import kotlin.reflect.KClass

class GroupModelInterpreter(val number: Int,
                            val interpreter: PatternInterpreter) : PatternInterpreter.InterpretedObjectFactory {
    override val interpretedTypePrediction: KClass<*>
        get() = GroupModelSpecification::class

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: List<InterpretationContext>) =
            GroupModelSpecificationBase(
                    name = "Unnamed Group ModelSpecification",
                    number = if (capturings["number"] == null) number
                    else interpreter.interpret<Int>(capturings["number"]!!, context),
                    memberModelType = interpreter.interpret<ModelSpecification>(singularize(capturings["member_type"]!!), context)
            )

    private fun singularize(phraseTree: PhraseTree): PhraseTree {
        return PhraseTree(phraseTree.rootWord.singularize(), when (phraseTree.nature) {
            "NNS" -> "NN"
            else -> phraseTree.nature
        }, phraseTree.role, phraseTree.children)
    }


}