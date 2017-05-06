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
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InterpretationContext
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Ignore
import org.junit.Test
import java.awt.Color

val BROWN = Color(165, 42, 42)


private fun interpretCreateComponent(text: String): ModelSpecification {

    val knowledge = Knowledge.knowledgeWithPrimitives()
    val interpreter = createDefaultModelInterpreter(knowledge)

    val result = interpreter.interpret<Any?>(
            SyntaxNetLink.parse(text),
            context = emptyList<InterpretationContext>())

    assertTrue(result is CreateCommand)

    val stmt = result as CreateCommand

    return stmt.what
}

class InterpreterTest {
    @Test
    fun interpreterTest1() {

        val newComponent = interpretCreateComponent("Create a cube.")

        val model = (newComponent as PrimitiveDerivative).base

        assertTrue(model is PrimitiveModelSpecification)
        assertEquals(PrimitiveModelSpecification.ShapeType.CUBE, model.shape)
        assertEquals(PrimitiveModelSpecification.ShapeType.CUBE, newComponent.shape)

    }

    @Test
    fun interpreterSizeTest() {

        val instance = interpretCreateComponent("Create a big cube.")

        assertTrue(instance.size.vertical > Length(AbsoluteScalar(1.0), LengthUnit.METER))
        assertTrue(instance.size.longitudinal > Length(AbsoluteScalar(1.0), LengthUnit.METER))
        assertTrue(instance.size.lateral > Length(AbsoluteScalar(1.0), LengthUnit.METER))

    }

    @Test
    @Ignore("Non-deterministic numbers not yet supported")
    fun interpreterTest2() {

        val scene = CompositeModelSpecificationBase(name = "Scene")

        val result = MainInterpreter().interpret("Add a cylinder or two.", scene)

        assertTrue(result is CreateCommand)

        val stmt = result as CreateCommand

        val model = stmt.what

        assertTrue(model is GroupModelSpecification)

        assertEquals(1, (model as GroupModelSpecification).number.toLong())

        assertEquals("Cylinder", model.memberModelType.name)

    }

    @Test
    fun coloredCube() {

        val cube = interpretCreateComponent("Create a red cube.")

        assertTrue(cube is PrimitiveDerivative)
        assertEquals(PrimitiveModelSpecification.ShapeType.CUBE,
                (cube as PrimitiveModelSpecification).shape)

        assertEquals(Color.RED,
                cube.color)
    }

    @Test
    fun createCubeAboveSphere() {

        listOf("A cube above a sphere",
                "Create a cube above a sphere.").forEach { phrase ->

            val cube = interpretCreateComponent(phrase)

            assertTrue(cube is PrimitiveDerivative)
            assertEquals(PrimitiveModelSpecification.ShapeType.CUBE,
                    (cube as PrimitiveModelSpecification).shape)

            assertTrue(cube.location is RelativeLocation)

            val loc = cube.location as RelativeLocation

            assertEquals(ABOVE, loc.relPos)

            assertEquals(PrimitiveModelSpecification.ShapeType.SPHERE, (loc.right as PrimitiveDerivative).shape)
        }
    }

    @Test
    fun createSphereAboveSphereWithDistance() {

        val cube = interpretCreateComponent("A cube 5 units above a sphere")

        assertTrue(cube is PrimitiveDerivative)
        assertEquals(PrimitiveModelSpecification.ShapeType.CUBE,
                (cube as PrimitiveModelSpecification).shape)

        assertTrue(cube.location is RelativeLocation)

        val loc = cube.location as RelativeLocation

        assertEquals(ABOVE, loc.relPos)
        assertEquals(5.0, (loc.dist as FixedDistance).distance, 0.01)

    }

    @Test
    fun createSphereStack() {

        val model = interpretCreateComponent("500 cubes above each other")

        assertTrue(model is GroupModelSpecification)

        assertTrue((model as GroupModelSpecification).memberModelType is PrimitiveModelSpecification)

        assertEquals(500, model.number)

    }

    @Test
    fun createLongBeam() {

        val model = interpretCreateComponent("a long cube")

        assertTrue(model is PrimitiveModelSpecification)

        assertTrue(model.size.lateral > Length(AbsoluteScalar(1.0), LengthUnit.METER))
        assertTrue(model.size.vertical == Length(AbsoluteScalar(1.0), LengthUnit.METER))
        assertTrue(model.size.longitudinal == Length(AbsoluteScalar(1.0), LengthUnit.METER))

    }

    @Test
    fun searchModel() {

        val knowledge = Knowledge.knowledgeWithPrimitives()
        val interpreter = createDefaultModelInterpreter(knowledge)

        val sphere = knowledge.getObject("sphere")

        val cube = knowledge.getObject("cube")

        val bigCube = knowledge.getObject("cube")
                .derive("The Block").apply {
            size *= AbsoluteScalar(2.0)
        }

        val model = CompositeModelSpecificationBase(name = "scene",
                directComponents = mutableSetOf(sphere, cube, bigCube))

        assertEquals(bigCube,
                interpreter.interpret<ModelSpecification>(SyntaxNetLink.parse("the blockyest"), emptyList()))

        assertEquals(bigCube,
                interpreter.interpret<ModelSpecification>(SyntaxNetLink.parse("the big cube"), emptyList()))

        assertEquals(cube,
                interpreter.interpret<ModelSpecification>(SyntaxNetLink.parse("the small cube"), emptyList()))

        assertEquals(sphere,
                interpreter.interpret<ModelSpecification>(SyntaxNetLink.parse("that sphere"), emptyList()))

        assertEquals(sphere,
                interpreter.interpret<ModelSpecification>(SyntaxNetLink.parse("that spherical object"), emptyList()))

        bigCube.location = RelativeLocation(cube, ABOVE, Distance.ANY)
        cube.location = RelativeLocation(sphere, ABOVE, Distance.ANY)

        assertEquals(sphere,
                interpreter.interpret<ModelSpecification>(SyntaxNetLink.parse("the topmost object"), emptyList()))


    }

    @Test
    fun onEachTest() {

        val knowledge = Knowledge.knowledgeWithPrimitives()
        val interpreter = createDefaultModelInterpreter(knowledge)

        val scene = CompositeModelSpecificationBase()

        val spheres = GroupModelSpecificationBase(name = "spheres",
                memberModelType = knowledge.getObject("sphere"),
                number = 15)

        scene.directComponents.add(spheres)

        val result = interpreter.interpret<CreateCommand>(
                SyntaxNetLink.parse("Put a small cube on top of each sphere."),
                context = listOf<InterpretationContext>(
                        DescriptionSession.DescriptionSessionContext(listOf(), scene)
                )
        )

        val forEachGroup = result.what

        assertTrue(forEachGroup is GroupModelSpecification)

        assertEquals(15, (forEachGroup as GroupModelSpecification).number)

        /*assertTrue(
            0.until(1).map { forEachGroup.totalSpecificationForMemberWithIndex(it) }
                    .all {
                        it.location.right == spheres.
                    }

    )*/
    }
}