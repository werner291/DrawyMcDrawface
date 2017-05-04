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

import nl.wernerkroneman.Drawy.ModelEditor.Knowledge
import org.junit.Test

@Test
fun createVillage() {

    val script = listOf(
            "Let there be a valley.",
            "There is a river flowing along the valley.",
            "There is a road running along the river.",
            "There is a group of houses along the road.",
            "Add a church near the houses, also along the road.",
            "Build a narrow wooden bridge over the river near the buildings.",
            "Put some cows in the fields near the buildings.",
            "Add some woods along the edges of the valley.")

    val result = runScript(script, Knowledge.knowledgeWithPrimitives())

}