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

package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.ModelEditor.Commands.CreateCommand
import nl.wernerkroneman.Drawy.Modelling.*
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.ABOVE
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Ignore
import org.junit.Test

class InterpreterTest {

    private fun interpretCreateComponent(text: String): Model {

        val result = MainInterpreter().interpreter.interpret<Any?>(
                SyntaxNetLink.parse(text),
                context = emptyList<InterpretationContext>())

        assertTrue(result is CreateCommand)

        val stmt = result as CreateCommand

        return stmt.what
    }

    @Test
    fun interpreterTest1() {

        val newComponent = interpretCreateComponent("Create a cube.")

        val model = (newComponent as PrimitiveDerivative).base

        assertTrue(model is PrimitiveModel)
        assertEquals(PrimitiveModel.ShapeType.CUBE, model.shape)
        assertEquals(PrimitiveModel.ShapeType.CUBE, newComponent.shape)

    }

    @Test
    fun interpreterSizeTest() {

        val instance = interpretCreateComponent("Create a big cube.")

        assertEquals(BIG, instance.size)

    }

    @Test
    @Ignore("Non-deterministic numbers not yet supported")
    fun interpreterTest2() {

        val scene = CompositeModelBase("Scene")

        val result = MainInterpreter().interpret("Add a cylinder or two.", scene)

        assertTrue(result is CreateCommand)

        val stmt = result as CreateCommand

        val model = stmt.what

        assertTrue(model is GroupModel)

        assertEquals(1, (model as GroupModel).number.toLong())

        assertEquals("Cylinder", model.memberModelType.name)

    }

    @Test
    fun createCubeAboveSphere() {

        val cube = interpretCreateComponent("A cube above a sphere")

        assertTrue(cube is PrimitiveDerivative)
        assertEquals(PrimitiveModel.ShapeType.CUBE,
                (cube as PrimitiveModel).shape)

        assertTrue(cube.location is RelativeLocation)

        val loc = cube.location as RelativeLocation

        assertEquals(ABOVE, loc.relPos)

        assertEquals(PrimitiveModel.ShapeType.SPHERE, (loc.right as PrimitiveDerivative).shape)

    }

    @Test
    fun createSphereAboveSphereWithDistance() {

        val cube = interpretCreateComponent("A cube 5 units above a sphere")

        assertTrue(cube is PrimitiveDerivative)
        assertEquals(PrimitiveModel.ShapeType.CUBE,
                (cube as PrimitiveModel).shape)

        assertTrue(cube.location is RelativeLocation)

        val loc = cube.location as RelativeLocation

        assertEquals(ABOVE, loc.relPos)
        assertEquals(5.0, (loc.dist as FixedDistance).distance, 0.01)

    }

    @Test
    fun createSphereStack() {

        val model = interpretCreateComponent("500 cubes above each other")

        assertTrue(model is GroupModel)

        assertTrue((model as GroupModel).memberModelType is PrimitiveModel)

        assertEquals(500, model.number)

    }

    @Test
    fun createSnowmanShape() {

        //val scene = CompositeModel("Scene")

        //val result = MainInterpreter().interpret("a small sphere on top of a big sphere on top of a bigger sphere", scene)

        //assertNotNull(result)
    }
}