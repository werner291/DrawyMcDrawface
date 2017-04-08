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
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.ABOVE
import org.junit.Assert.*
import org.junit.Ignore
import org.junit.Test

class InterpreterTest {

    private fun interpretCreateComponent(text: String): SceneComponent.NewComponent {
        val scene = CompositeModel("Scene")
        val result = MainInterpreter().interpret(text, scene)

        assertTrue(result is CreateEntityEditorCommand)

        val stmt = result as CreateEntityEditorCommand

        val newComponent = stmt.what as SceneComponent.NewComponent
        return newComponent
    }

    @Test
    fun interpreterTest1() {

        val newComponent = interpretCreateComponent("Create a cube.")
        val model = newComponent.model

        assertTrue(model is PrimitiveModel)
        assertEquals("Cube", model.name)

    }

    @Test
    fun interpreterSizeTest() {

        val newComponent = interpretCreateComponent("Create a big cube.")
        val model = newComponent.model

        assertTrue(model is VariantModel)
        assertTrue((model as VariantModel).modifiers.any({ it is SizeModifier }))

        val cube = model.base

        assertEquals("Cube", cube.name)

    }

    @Test
    @Ignore("Non-deterministic numbers not yet supported")
    fun interpreterTest2() {

        val scene = CompositeModel("Scene")

        val result = MainInterpreter().interpret("Add a cylinder or two.", scene)

        assertTrue(result is CreateEntityEditorCommand)

        val stmt = result as CreateEntityEditorCommand

        val model = (stmt.what as SceneComponent.NewComponent).model

        assertTrue(model is GroupModel)

        assertEquals(1, (model as GroupModel).number.toLong())

        assertEquals("Cylinder", model.memberModelType.name)

    }

    @Test
    fun createCubeAboveSphere() {

        val newComponent = interpretCreateComponent("A cube above a sphere")
        val model = newComponent.model

        assertTrue(model is PrimitiveModel)
        assertTrue(model.name == "Cube")

        assertEquals(1, newComponent.relations.size)

        val relation = newComponent.relations.first()

        assertEquals(ABOVE, relation.relPos)

        assertTrue(relation.right is SceneComponent.NewComponent)
        assertTrue((relation.right as SceneComponent.NewComponent).model is PrimitiveModel)
        assertEquals("Sphere", (relation.right as SceneComponent.NewComponent).model.name)

    }

    @Test
    fun createSphereAboveSphereWithDistance() {

        val newComponent = interpretCreateComponent("A cube 5 units above a sphere")
        val model = newComponent.model

        assertTrue(model is PrimitiveModel)
        assertTrue(model.name == "Cube")

        assertEquals(1, newComponent.relations.size)

        val relation = newComponent.relations.first()

        assertEquals(ABOVE, relation.relPos)

        assertTrue(relation.right is SceneComponent.NewComponent)
        assertTrue((relation.right as SceneComponent.NewComponent).model is PrimitiveModel)
        assertEquals("Sphere", (relation.right as SceneComponent.NewComponent).model.name)

    }

    @Test
    fun createSphereStack() {

        val newComponent = interpretCreateComponent("500 cubes above each other")
        val model = newComponent.model

        assertTrue(model is GroupModel)

        assertTrue((model as GroupModel).memberModelType is PrimitiveModel)

        assertEquals(500, model.number.toLong())

    }

    @Test
    fun createSnowmanShape() {

        val scene = CompositeModel("Scene")

        val result = MainInterpreter().interpret("a small sphere on top of a big sphere on top of a bigger sphere", scene)

        assertNotNull(result)
    }
}