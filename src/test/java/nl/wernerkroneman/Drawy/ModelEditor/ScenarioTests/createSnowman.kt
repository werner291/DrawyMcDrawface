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
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import java.awt.Color.ORANGE
import java.awt.Color.WHITE

@Test
fun createSnowman() {

    // Init knowledge
    val knowledge = Knowledge.knowledgeWithPrimitives()

    val script = listOf(
            "A snowball is a white sphere.",
            "A carror is an elongated orange cone.",
            "A rock is a dark gray sphere.",
            "A pebble is a small rock.",
            "A stick is a cylinder 1 meter long and 10 cm in diameter.",
            "Create a big snowball.",
            "Put a smaller snowball on top of it.",
            "Put a smaller snowball on top of that.",
            "That last snowball is the head.",
            "Stick a carrot into the front of the head.",
            "This is the nose.",
            "Stick a pair of small pebbles into the head above the nose for the eyes.",
            "Insert a wooden stick into each side of the middle snowball for the arms.")

    val result = runScript(script, knowledge)

    // At least 3 snowballs
    val _isSnowball = { it: ModelSpecification ->
        it is PrimitiveModelSpecification
                && it.shape == PrimitiveModelSpecification.ShapeType.SPHERE
                && it.color == WHITE
    }

    assertTrue(result.directComponents.count(_isSnowball) >= 3)

    // Has a carrot as a nose
    assertTrue(result.directComponents.any {
        it is PrimitiveModelSpecification
                && it.shape == PrimitiveModelSpecification.ShapeType.CONE
                && it.color == ORANGE
                && it.location is RelativeLocation
                && it.name.contains("nose")
                && (it.location as RelativeLocation).relPos == RelativePositionConstraint.FRONT
                && _isSnowball((it.location as RelativeLocation).right)
    })

    val arms = result.components.filter({
        it is PrimitiveModelSpecification
                && it.shape == PrimitiveModelSpecification.ShapeType.CYLINDER
                && it.color == BROWN
                && it.location is RelativeLocation
                && _isSnowball((it.location as RelativeLocation).right)
    })

    // Has a carrot as a nose
    assertEquals(2, arms.count())
    assertTrue(arms.any { (it.location as RelativeLocation).relPos == RelativePositionConstraint.RIGHT })
    assertTrue(arms.any { (it.location as RelativeLocation).relPos == RelativePositionConstraint.LEFT })

    val eyes = result.directComponents.first { it is GroupModelSpecification } as GroupModelSpecification

    // Has a carrot as a nose
    assertEquals(2, eyes.number)
    assertEquals(RelativePositionConstraint.FRONT, (eyes.location as RelativeLocation).relPos)
    assertTrue((eyes.location as RelativeLocation).right.run {
        _isSnowball(this) && name.contains("head")
    })

}