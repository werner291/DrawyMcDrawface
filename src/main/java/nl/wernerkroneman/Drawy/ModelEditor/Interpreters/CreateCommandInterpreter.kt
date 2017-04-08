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

import nl.wernerkroneman.Drawy.ModelEditor.*
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
        get() = CreateEntityEditorCommand::class

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: List<InterpretationContext>): CreateEntityEditorCommand {
        val what = interpreter.interpret(
                capturings["what"]!!,
                SceneComponent::class,
                context + CreateCommandContext()
        ) as SceneComponent.NewComponent

        val where = capturings["where"]

        val extraRelations = if (where != null)
            setOf(interpreter.interpret(
                    where,
                    SceneComponent::class,
                    context + CreateCommandContext()
            ) as SceneComponentRelation)
        else emptySet()

        return CreateEntityEditorCommand(
                target = {
                    (context.findLast({ it is DescriptionSession.DescriptionSessionContext })
                            as DescriptionSession.DescriptionSessionContext).scene
                },
                previous = context.findLast { it is EditorCommand } as EditorCommand?,
                what = SceneComponent.NewComponent(what.model, what.relations + extraRelations)
        )
    }

    class CreateCommandContext : InterpretationContext
}
