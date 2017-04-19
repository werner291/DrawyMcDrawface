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
import nl.wernerkroneman.Drawy.ModelEditor.Commands.EditorCommand
import nl.wernerkroneman.Drawy.Modelling.*
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.ABOVE
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.BELOW
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.FRONT
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.LEFT
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.RIGHT
import nl.wernerkroneman.Drawy.ParseTreeMatcher.InterpretationContext
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Ignore
import org.junit.Test
import java.awt.Color
import java.awt.Color.*

val BROWN = Color(165, 42, 42)

class InterpreterTest {

    private fun interpretCreateComponent(text: String): Model {

        val interpreter = createDefaultModelInterpreter()
        val knowledge = Knowledge.knowledgeWithPrimitives()

        val result = interpreter.interpret<Any?>(
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

        assertTrue(instance.size.y > Length(AbsoluteScalar(1.0), LengthUnit.METER))
        assertTrue(instance.size.z > Length(AbsoluteScalar(1.0), LengthUnit.METER))
        assertTrue(instance.size.x > Length(AbsoluteScalar(1.0), LengthUnit.METER))

    }

    @Test
    @Ignore("Non-deterministic numbers not yet supported")
    fun interpreterTest2() {

        val scene = CompositeModelBase(name = "Scene")

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

        listOf("A cube above a sphere",
                "Create a cube above a sphere.").forEach { phrase ->

            val cube = interpretCreateComponent(phrase)

            assertTrue(cube is PrimitiveDerivative)
            assertEquals(PrimitiveModel.ShapeType.CUBE,
                    (cube as PrimitiveModel).shape)

            assertTrue(cube.location is RelativeLocation)

            val loc = cube.location as RelativeLocation

            assertEquals(ABOVE, loc.relPos)

            assertEquals(PrimitiveModel.ShapeType.SPHERE, (loc.right as PrimitiveDerivative).shape)
        }
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
    fun createLongBeam() {

        val model = interpretCreateComponent("a long cube")

        assertTrue(model is PrimitiveModel)

        assertTrue(model.size.x > Length(AbsoluteScalar(1.0), LengthUnit.METER))
        assertTrue(model.size.y == Length(AbsoluteScalar(1.0), LengthUnit.METER))
        assertTrue(model.size.z == Length(AbsoluteScalar(1.0), LengthUnit.METER))

    }

    @Test
    fun searchModel() {

        val interpreter = createDefaultModelInterpreter()
        val knowledge = Knowledge.knowledgeWithPrimitives()

        val sphere = knowledge.getObject("sphere")!!

        val cube = knowledge.getObject("cube")!!

        val bigCube = knowledge.getObject("cube")!!.derive("The Block").apply {
            size *= AbsoluteScalar(2.0)
        }

        val model = CompositeModelBase(name = "scene",
                components = mutableSetOf(sphere, cube, bigCube))

        assertEquals(bigCube,
                interpreter.interpret<Model>(SyntaxNetLink.parse("the blockyest"), emptyList()))

        assertEquals(bigCube,
                interpreter.interpret<Model>(SyntaxNetLink.parse("the big cube"), emptyList()))

        assertEquals(cube,
                interpreter.interpret<Model>(SyntaxNetLink.parse("the small cube"), emptyList()))

        assertEquals(sphere,
                interpreter.interpret<Model>(SyntaxNetLink.parse("that sphere"), emptyList()))

        assertEquals(sphere,
                interpreter.interpret<Model>(SyntaxNetLink.parse("that spherical object"), emptyList()))

        bigCube.location = RelativeLocation(cube, ABOVE, Distance.ANY)
        cube.location = RelativeLocation(sphere, ABOVE, Distance.ANY)

        assertEquals(sphere,
                interpreter.interpret<Model>(SyntaxNetLink.parse("the topmost object"), emptyList()))


    }

    @Test
    fun createSnowman() {

        // Init knowledge
        val knowledge = Knowledge.knowledgeWithPrimitives()

        knowledge.remember(knowledge.getObject("sphere")!!
                .derive("snowball")
                .apply { color = WHITE })

        knowledge.remember(knowledge.getObject("cone")!!
                .derive("carrot")
                .apply { color = ORANGE })

        knowledge.remember(knowledge.getObject("sphere")!!
                .derive("rock").apply { color = DARK_GRAY })

        knowledge.remember(knowledge.getObject("rock")!!
                .derive("pebble")
                .apply { size = AbsoluteSize(Length(AbsoluteScalar(0.1), LengthUnit.METER)) })

        knowledge.remember(knowledge.getObject("cylinder")!!
                .derive("stick")
                .apply {
                    size = AbsoluteSize(
                            x = Length(AbsoluteScalar(0.1), LengthUnit.METER),
                            y = Length(AbsoluteScalar(0.1), LengthUnit.METER),
                            z = Length(AbsoluteScalar(1.0), LengthUnit.METER))
                })

        val script = listOf(
                "Create a big snowball.",
                "Put a smaller snowball on top of it.",
                "Put a smaller snowball on top of that.",
                "That last snowball is the head.",
                "Stick a carrot into the front of the head.",
                "This is the nose.",
                "Stick a pair of small pebbles into the head above the nose for the eyes.",
                "Insert two wooden sticks into the sides of the middle snowball for the arms.")

        val result = runScript(script, knowledge)

        fun isSnowball(it: Model): Boolean {
            return it is PrimitiveModel
                    && it.shape == PrimitiveModel.ShapeType.SPHERE
                    && it.color == WHITE
        }

        // At least 3 snowballs
        assertTrue(result.components.count(::isSnowball) >= 3)

        // Has a carrot as a nose
        assertTrue(result.components.any {
            it is PrimitiveModel
                    && it.shape == PrimitiveModel.ShapeType.CONE
                    && it.color == ORANGE
                    && it.location is RelativeLocation
                    && it.name.contains("nose")
                    && (it.location as RelativeLocation).relPos == FRONT
                    && isSnowball((it.location as RelativeLocation).right)
        })

        val arms = result.components.filter {
            it is PrimitiveModel
                    && it.shape == PrimitiveModel.ShapeType.CYLINDER
                    && it.color == BROWN
                    && it.location is RelativeLocation
                    && isSnowball((it.location as RelativeLocation).right)
        }

        // Has a carrot as a nose
        assertEquals(2, arms.count())
        assertTrue(arms.any { (it.location as RelativeLocation).relPos == RIGHT })
        assertTrue(arms.any { (it.location as RelativeLocation).relPos == LEFT })

        val eyes = result.components.first { it is GroupModel } as GroupModel

        // Has a carrot as a nose
        assertEquals(2, eyes.number)
        assertEquals(FRONT, (eyes.location as RelativeLocation).relPos)
        assertTrue((eyes.location as RelativeLocation).right.run {
            isSnowball(this) && name.contains("head")
        })

    }

    private fun runScript(script: List<String>, knowledge: Knowledge): CompositeModel {

        val interpreter = createDefaultModelInterpreter(knowledge)

        val commands = mutableListOf<EditorCommand>()

        val result = CompositeModelBase(name = "Scene")

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

    @Test @Ignore("Linear objects not yet supported.")
    fun createSwingset() {

        val script = listOf(
                "Create a 3-meter-long wooden beam 2 meters above the ground supported by a 2-meter-tall wooden posts.",
                "Attach a two pieces of rope to the horizontal wooden beam 0.5 meters apart.",
                "Create a small wooden plank, and attach the loose ends of the rope to both ends of the plank.")


        val result = runScript(script, Knowledge.knowledgeWithPrimitives())

        assertTrue(result.components.any {
            it is PrimitiveModel &&
                    it.color == BROWN &&
                    it.size.x == Length(AbsoluteScalar(3.0), LengthUnit.METER)
        })

        assertTrue(result.components.any {
            it is PrimitiveModel &&
                    it.color == BROWN &&
                    it.size.z == Length(AbsoluteScalar(2.0), LengthUnit.METER) &&
                    it.location is RelativeLocation &&
                    (it.location as RelativeLocation).relPos == BELOW &&
                    (it.location as RelativeLocation).dist == FixedDistance(0.0)
        })

        TODO("Cannot model ropes yet.")


    }
}
