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

import nl.wernerkroneman.Drawy.Modelling.*
import org.junit.Assert
import org.junit.Ignore
import org.junit.Test

class InterpreterTest {

    @Test
    fun interpreterTest1() {

        val scene = CompositeModel("Scene")
        val result = MainInterpreter().interpret("Create a cube.", scene)

        Assert.assertTrue(result is CreateEntityEditorCommand)

        val stmt = result as CreateEntityEditorCommand

        Assert.assertTrue(stmt.what is PrimitiveModel)

        Assert.assertEquals("Cube", stmt.what!!.name)

    }

    @Test
    @Ignore("Non-deterministic numbers not yet supported")
    fun interpreterTest2() {

        val scene = CompositeModel("Scene")

        val result = MainInterpreter().interpret("Add a cylinder or two.", scene)

        Assert.assertTrue(result is CreateEntityEditorCommand)

        val stmt = result as CreateEntityEditorCommand

        Assert.assertTrue(stmt.what is GroupModel)

        Assert.assertEquals(1, (stmt.what as GroupModel).number.toLong())

        Assert.assertEquals("Cylinder", (stmt.what as GroupModel).memberModelType.name)

    }

    @Test
    fun createCubeAboveSphere() {

        val scene = CompositeModel("Scene")

        val result = MainInterpreter().interpret("A cube above a sphere", scene)

        Assert.assertNotNull(result)

        // ----------------------

        Assert.assertTrue(result is CreateEntityEditorCommand)

        val stmtA = result as CreateEntityEditorCommand

        Assert.assertTrue(stmtA.what is CompositeModel)

        Assert.assertTrue((stmtA.what as CompositeModel).components
                .all({ it.model is PrimitiveModel }))

        Assert.assertEquals(1, (stmtA.what as CompositeModel).constraints.size.toLong())

        val constraint = (stmtA.what as CompositeModel).constraints.iterator().next()
        Assert.assertTrue(constraint is RelativePositionConstraint)

        Assert.assertEquals(RelativePositionConstraint.ABOVE, (constraint as RelativePositionConstraint).pos)

        Assert.assertEquals(Distance.ANY, constraint.dist)

        stmtA.apply()

        // ----------------------

        Assert.assertEquals(1, scene.components.size.toLong())

    }

    @Test
    fun createSphereAboveSphereWithDistance() {

        val scene = CompositeModel("Scene")

        val result = MainInterpreter().interpret("A cube 5 units above a sphere", scene)

        Assert.assertNotNull(result)

        // ----------------------

        Assert.assertTrue(result is CreateEntityEditorCommand)

        val stmtA = result as CreateEntityEditorCommand

        Assert.assertTrue(stmtA.what is CompositeModel)

        Assert.assertTrue((stmtA.what as CompositeModel).components
                .all({ it.model is PrimitiveModel }))

        Assert.assertEquals(1, (stmtA.what as CompositeModel).constraints.size.toLong())

        val constraint = (stmtA.what as CompositeModel).constraints.iterator().next()
        Assert.assertTrue(constraint is RelativePositionConstraint)

        val positionConstraint = constraint as RelativePositionConstraint
        Assert.assertEquals(RelativePositionConstraint.ABOVE, positionConstraint.pos)

        Assert.assertTrue(positionConstraint.dist is FixedDistance)
        Assert.assertEquals(5.0, (positionConstraint.dist as FixedDistance).distance, 0.01)

        stmtA.apply()

        // ----------------------

        Assert.assertEquals(1, scene.components.size.toLong())

    }

    @Test
    fun createSphereStack() {

        val scene = CompositeModel("Scene")

        val result = MainInterpreter().interpret("500 cubes above each other", scene)

        Assert.assertNotNull(result)

        // ----------------------

        Assert.assertTrue(result is CreateEntityEditorCommand)

        val stmtA = result as CreateEntityEditorCommand

        Assert.assertTrue(stmtA.what is GroupModel)

        Assert.assertTrue((stmtA.what as GroupModel).memberModelType is PrimitiveModel)

        Assert.assertEquals(500, (stmtA.what as GroupModel).number.toLong())

        stmtA.apply()

    }
}