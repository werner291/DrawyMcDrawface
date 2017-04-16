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

import nl.wernerkroneman.Drawy.ModelEditor.Commands.CreateCommand
import nl.wernerkroneman.Drawy.ModelEditor.Commands.EditorCommand
import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSession
import nl.wernerkroneman.Drawy.ModelEditor.InterpretationContext
import nl.wernerkroneman.Drawy.Modelling.Model
import nl.wernerkroneman.Drawy.Modelling.RelativeLocation
import nl.wernerkroneman.Drawy.Modelling.combineLocations
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import kotlin.reflect.KClass

/**
 * Generate a creation command for the object(s) described in this sentence part.
 * @param scene A supplier that provides a scene when executed in which to create the object
 */
class CreateCommandInterpreter(private val interpreter: PatternInterpreter)
    : PatternInterpreter.InterpretedObjectFactory {

    override val interpretedTypePrediction: KClass<*>
        get() = CreateCommand::class

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: List<InterpretationContext>): CreateCommand {

        val what = interpreter.interpret<Model>(
                capturings["what"]!!,
                context + CreateCommandContext()
        )

        val where = capturings["where"]

        if (where != null) {
            what.location = combineLocations(what.location, interpreter.interpret<RelativeLocation>(
                    where,
                    context + CreateCommandContext()
            ))
        }

        return CreateCommand(
                target = {
                    (context.findLast({ it is DescriptionSession.DescriptionSessionContext })
                            as DescriptionSession.DescriptionSessionContext).scene
                },
                previous = context.findLast { it is EditorCommand } as EditorCommand?,
                what = what
        )
    }

    class CreateCommandContext : InterpretationContext
}
