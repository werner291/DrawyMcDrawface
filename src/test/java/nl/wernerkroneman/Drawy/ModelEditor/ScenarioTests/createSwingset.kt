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

import nl.wernerkroneman.Drawy.ModelEditor.BROWN
import nl.wernerkroneman.Drawy.ModelEditor.Knowledge
import nl.wernerkroneman.Drawy.Modelling.*
import org.junit.Assert.assertTrue
import org.junit.Ignore
import org.junit.Test

@Test @Ignore("Linear objects not yet supported.")
fun createSwingset() {

    val script = listOf(
            "Create a 3-meter-long wooden beam 2 meters above the ground supported by a 2-meter-tall wooden posts.",
            "Attach a two pieces of rope to the horizontal wooden beam 0.5 meters apart.",
            "Create a small wooden plank, and attach the loose ends of the rope to both ends of the plank.")


    val result = runScript(script, Knowledge.knowledgeWithPrimitives())

    assertTrue(result.directComponents.any {
        it is PrimitiveModelSpecification &&
                it.color == BROWN &&
                it.size.lateral == Length(AbsoluteScalar(3.0), LengthUnit.METER)
    })

    assertTrue(result.directComponents.any {
        it is PrimitiveModelSpecification &&
                it.color == BROWN &&
                it.size.longitudinal == Length(AbsoluteScalar(2.0), LengthUnit.METER) &&
                it.location is RelativeLocation &&
                (it.location as RelativeLocation).relPos == RelativePositionConstraint.BELOW &&
                (it.location as RelativeLocation).dist == FixedDistance(0.0)
    })

    TODO("Cannot model ropes yet.")
}