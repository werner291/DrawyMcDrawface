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

import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSession
import nl.wernerkroneman.Drawy.Modelling.Model
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InterpretationContext
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter.InterpretedObjectFactory
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import kotlin.reflect.KClass

class FindModelInterpreter : InterpretedObjectFactory {

    override val interpretedTypePrediction: KClass<*>
        get() = Model::class

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: List<InterpretationContext>): Model {

        val query = capturings["name"]!!.rootWord

        val descrSess = context.last { it is DescriptionSession.DescriptionSessionContext }
                as DescriptionSession.DescriptionSessionContext

        return descrSess.scene.components.find { it.name.contains(query) }!!

    }

}