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

package nl.wernerkroneman.Drawy.ModelEditor.ScenarioTests

import nl.wernerkroneman.Drawy.ModelEditor.Commands.EditorCommand
import nl.wernerkroneman.Drawy.ModelEditor.DescriptionSession
import nl.wernerkroneman.Drawy.ModelEditor.Knowledge
import nl.wernerkroneman.Drawy.ModelEditor.SyntaxNetLink
import nl.wernerkroneman.Drawy.ModelEditor.createDefaultModelInterpreter
import nl.wernerkroneman.Drawy.Modelling.CompositeModelSpecification
import nl.wernerkroneman.Drawy.Modelling.CompositeModelSpecificationBase

fun runScript(script: List<String>,
              knowledge: Knowledge): CompositeModelSpecification {

    val interpreter = createDefaultModelInterpreter(knowledge)

    val commands = mutableListOf<EditorCommand>()

    val result = CompositeModelSpecificationBase(name = "Scene")

    for (line in script) {
        println("Interpreting: $line")
        val interpreted = interpreter.interpret<EditorCommand>(
                SyntaxNetLink.parse(line),
                listOf(
                        DescriptionSession.DescriptionSessionContext(
                                commands, result
                        )
                )
        )

        interpreted.apply()

        commands.add(interpreted)
    }
    return result
}