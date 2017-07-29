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

package nl.wernerkroneman.Drawy.ConcreteModelling

import nl.wernerkroneman.Drawy.AbstractToConcreteConverter.AbstractToConcrete
import nl.wernerkroneman.Drawy.Modelling.*
import org.joml.Vector3d
import org.junit.Assert
import org.junit.Test

class AbstractToConcreteTest {

    // This should produce a single cube centered at (0,0,0) of edge length 1.
    val cube = ModelSpecification(shape = Shape.CUBE)

    @Test
    @Throws(Exception::class)
    fun computeSceneSingleCube() {

        val converter = AbstractToConcrete(DefaultMeshFactory())

        val box = converter.concreteForModel(cube).aabb

        val center = box.center
        Assert.assertEquals(0.0, center.x, 0.01)
        Assert.assertEquals(0.0, center.y, 0.01)
        Assert.assertEquals(0.0, center.z, 0.01)

        Assert.assertEquals(1.0, box.sizeX, 0.01)
        Assert.assertEquals(1.0, box.sizeY, 0.01)
        Assert.assertEquals(1.0, box.sizeZ, 0.01)
    }

    @Test
    @Throws(Exception::class)
    fun computeSceneSingleCubeInComposite() {
        // This should produce a single cube centered at (0,0,0) of edge lateral 1.
        val composite = ModelSpecification(has = listOf(cube))

        val result = AbstractToConcrete(DefaultMeshFactory()).concreteForModel(composite)

        Assert.assertEquals(1, result.children.size)
        Assert.assertEquals(1, result.children.first().drawables.size)

        val box = result.aabb

        val center = box.center
        Assert.assertEquals(0.0, center.x, 0.01)
        Assert.assertEquals(0.0, center.y, 0.01)
        Assert.assertEquals(0.0, center.z, 0.01)

        Assert.assertEquals(1.0, box.sizeY, 0.01)
        Assert.assertEquals(1.0, box.sizeZ, 0.01)
        Assert.assertEquals(1.0, box.sizeX, 0.01)
    }

    /*@Test
    @Throws(Exception::class)
    fun computeSceneMultipleCubeInComposite() {
        // This should produce multiple, non-overlapping cubes.
        for (rep in 0..49) {
            val cube = PrimitiveModelSpecificationBase(name = "Cube", shape = PrimitiveModelSpecification.ShapeType.CUBE)

            val composite = CompositeModelSpecificationBase(name = "Cube container.")

            val NUM_CUBES = 10

            for (i in 0..NUM_CUBES - 1) {
                composite.directComponents.add(cube.derive("Cube ${i + 1}").apply {
                    this.location = NOT_INTERSECTING_SOLID
                })
            }

            val converter = AbstractToConcrete(DefaultMeshFactory())

            val result = converter.computeScene(composite)

            Assert.assertEquals(0, result.rootSceneNode.getDrawables().size.toLong())
            Assert.assertEquals(NUM_CUBES.toLong(), result.rootSceneNode.getChildren().size.toLong())

            (0..NUM_CUBES - 1).forEach { i ->
                (0..NUM_CUBES - 1).forEach { j ->
                    if (i != j) {
                        val a = result.rootSceneNode.getChildren()[i].getDrawables()[0].worldAABB
                        val b = result.rootSceneNode.getChildren()[j].getDrawables()[0].worldAABB

                        Assert.assertFalse(a.intersects(b, 0.01))
                    }
                }
            }
        }
    }

    /**
     * Supposed to create a stack of 3 cubes.
     */
    @Test
    fun testRelativePositionConstraint() {
        for (rep in 0..99) {
            // Set up a simple scene with two cubes
            val cube = PrimitiveModelSpecificationBase(name = "Cube", shape = PrimitiveModelSpecification.ShapeType.CUBE)

            val composite = CompositeModelSpecificationBase(name = "Cube container.")

            // Create three cube directComponents.
            val cubeA = cube.derive("A")
            composite.directComponents.add(cubeA)

            val cubeB = cube.derive("B")
            composite.directComponents.add(cubeB)

            val cubeC = cube.derive("C")
            composite.directComponents.add(cubeC)

            cubeA.location = RelativeLocation(cubeB, ABOVE, Distance.ANY)
            cubeB.location = RelativeLocation(cubeC, ABOVE, Distance.ANY)

            // Compute the concrete scene
            val converter = AbstractToConcrete(DefaultMeshFactory())

            val scene = converter.computeScene(composite)

            // Should be three children
            val children = scene.rootSceneNode.getChildren()
            Assert.assertEquals(3, children.size.toLong())

            // Check for intersecting pairs
            for (i in 0..2) {
                for (j in 0..2) {
                    if (i != j) {
                        val boxA = children[i].computeWorldAABB()
                        val boxB = children[j].computeWorldAABB()
                        Assert.assertFalse(boxA.intersects(boxB, 0.01))
                    }
                }
            }

            // Check whether the bounding box is consistent with a stack of 3 cubes.
            val totalBox = scene.rootSceneNode.computeWorldAABB()
            Assert.assertEquals(3.0, totalBox.sizeY, 0.5)
            Assert.assertEquals(1.0, totalBox.sizeX, 0.1)
            Assert.assertEquals(1.0, totalBox.sizeZ, 0.1)
        }
    }

    /**
     * Supposed to create a stack of 3 cubes.
     */
    @Test
    fun testRelativePositionConstraintExactDistance() {
        for (rep in 0..49) {
            // Set up a simple scene with two cubes
            val cube = PrimitiveModelSpecificationBase(name = "Cube", shape = PrimitiveModelSpecification.ShapeType.CUBE)

            val composite = CompositeModelSpecificationBase(name = "Cube container.")

            // Create three cube directComponents.
            val cubeA = cube.derive("A")
            composite.directComponents.add(cubeA)

            val cubeB = cube.derive("B")
            composite.directComponents.add(cubeB)

            val cubeC = cube.derive("C")
            composite.directComponents.add(cubeC)

            cubeA.location = RelativeLocation(cubeB, ABOVE, FixedDistance(0.0))
            cubeB.location = RelativeLocation(cubeC, ABOVE, FixedDistance(0.0))

            // Compute the concrete scene
            val converter = AbstractToConcrete(DefaultMeshFactory())

            val scene = converter.computeScene(composite)

            // Should be three children
            val children = scene.rootSceneNode.getChildren()
            Assert.assertEquals(3, children.size.toLong())

            // Check for intersecting pairs
            for (i in 0..2) {
                for (j in 0..2) {
                    if (i != j) {
                        val boxA = children[i].computeWorldAABB()
                        val boxB = children[j].computeWorldAABB()
                        Assert.assertFalse(boxA.intersects(boxB, 0.01))
                    }
                }
            }

            // Check whether the bounding box is consistent with a stack of 3 cubes.
            val totalBox = scene.rootSceneNode.computeWorldAABB()
            Assert.assertEquals(3.0, totalBox.sizeY, 0.5)
            Assert.assertEquals(1.0, totalBox.sizeX, 0.1)
            Assert.assertEquals(1.0, totalBox.sizeZ, 0.1)
        }
    }*/

}