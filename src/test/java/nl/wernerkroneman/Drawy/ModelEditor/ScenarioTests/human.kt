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
import nl.wernerkroneman.Drawy.Modelling.AbsoluteScalar
import nl.wernerkroneman.Drawy.Modelling.Length
import nl.wernerkroneman.Drawy.Modelling.LengthUnit
import nl.wernerkroneman.Drawy.Modelling.PrimitiveModelSpecification
import org.junit.Test

@Test
fun human() {

    val result = runScript(listOf(
            "For the torso, create a rectangular cuboid which is 1 m high, about half a meter wide and 20 cm front-to-back.",
            "Attach a pair of long cylinders to the bottom, one on either side, to make the legs.",
            "Attach a pair of long cylinders towards the top on the left and right side of the torso.",
            "These are the arms.",
            "Attach an elongated sphere to the top of the torso in the middle.",
            "That sphere is the head."
    ), Knowledge.knowledgeWithPrimitives())

    result.components.any {
        it.name == "torso"
        it is PrimitiveModelSpecification
        it.size.vertical == Length(AbsoluteScalar(1.0), LengthUnit.METER)
        it.size.longitudinal == Length(AbsoluteScalar(0.2), LengthUnit.METER)
        it.size.lateral == Length(AbsoluteScalar(0.5), LengthUnit.METER)
    }


}
