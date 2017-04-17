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

import nl.wernerkroneman.Drawy.ModelEditor.Commands.RenameCommand
import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSession
import nl.wernerkroneman.Drawy.Modelling.Model
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InterpretationContext
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PatternInterpreter
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import kotlin.reflect.KClass

class RenameCommandInterpreter(private val interpreter: PatternInterpreter) : PatternInterpreter.InterpretedObjectFactory {

    override val interpretedTypePrediction: KClass<*>
        get() = RenameCommand::class

    override fun interpret(capturings: Map<String, PhraseTree>,
                           context: List<InterpretationContext>) =
            RenameCommand(
                    previous = (context.findLast { it is DescriptionSession.DescriptionSessionContext }
                            as DescriptionSession.DescriptionSessionContext)
                            .pastCommands.lastOrNull(),
                    newName = capturings["new name"]!!.rootWord,
                    target = interpreter.interpret<Model>(capturings["target"]!!, context)
            )


}