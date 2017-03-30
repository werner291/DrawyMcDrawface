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
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.Companion.ABOVE
import org.joml.Vector3d
import org.junit.Assert
import org.junit.Test

/**
 * Created by werner on 27-12-16.
 */
class AbstractToConcreteTest {

    /*@Test
    @Throws(Exception::class)
    fun findEmptyPlaceTest() {

        val meshFactory = DefaultMeshFactory()
        val primitiveGenerator = PrimitiveGenerator(meshFactory)

        for (i in 0..99) {
            val root = SceneNode()

            val child = SceneNode()
            root.addChild(child)

            child.addDrawable(Drawable(primitiveGenerator.generateUnitCube()))

            val converter = AbstractToConcrete(meshFactory)

            val doesNotIntersect = ArrayList<AABB>()
            doesNotIntersect.add(child.computeWorldAABB())

            val empty = PositionalSolver.findEmptyPlace(doesNotIntersect, 0.0, Vector3d(2.0),
                    AABB(Vector3d(java.lang.Double.POSITIVE_INFINITY),
                            Vector3d(java.lang.Double.NEGATIVE_INFINITY)))

            Assert.assertFalse(empty!!.intersects(AABB(Vector3d(0.5), Vector3d(-0.5)), 0.0))

            Assert.assertEquals(2.0, empty.sizeZ, 0.01)
            Assert.assertEquals(2.0, empty.sizeX, 0.01)
            Assert.assertEquals(2.0, empty.sizeY, 0.01)

            Assert.assertTrue(empty.getCenter(Vector3d()).length() < 10)

        }
    }*/

    @Test
    @Throws(Exception::class)
    fun computeSceneSingleCube() {
        // This should produce a single cube centered at (0,0,0) of edge length 1.

        val cube = PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube")

        val converter = AbstractToConcrete(DefaultMeshFactory())

        val result = converter.computeScene(cube)

        Assert.assertEquals(1, result.getRootSceneNode().getDrawables().size.toLong())

        val cubemesh = result.getRootSceneNode().getDrawables()[0].getMesh()

        val box = cubemesh.computeAABB()

        val center = box.getCenter(Vector3d())
        Assert.assertEquals(0.0, center.x, 0.01)
        Assert.assertEquals(0.0, center.y, 0.01)
        Assert.assertEquals(0.0, center.z, 0.01)

        Assert.assertEquals(1.0, box.sizeY, 0.01)
        Assert.assertEquals(1.0, box.sizeZ, 0.01)
        Assert.assertEquals(1.0, box.sizeX, 0.01)
    }

    @Test
    @Throws(Exception::class)
    fun computeSceneSingleCubeInComposite() {
        // This should produce a single cube centered at (0,0,0) of edge length 1.

        val cube = PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube")

        val composite = CompositeModel("Cube container.")

        composite.addComponentForModel(cube)

        val converter = AbstractToConcrete(DefaultMeshFactory())

        val result = converter.computeScene(composite)

        Assert.assertEquals(0, result.getRootSceneNode().getDrawables().size.toLong())
        Assert.assertEquals(1, result.getRootSceneNode().getChildren().size.toLong())

        val childNode = result.getRootSceneNode().getChildren()[0]
        Assert.assertEquals(1, childNode.getDrawables().size.toLong())

        val cubemesh = childNode.getDrawables()[0].getMesh()

        val box = cubemesh.computeAABB()

        val center = box.getCenter(Vector3d())
        Assert.assertEquals(0.0, center.x, 0.01)
        Assert.assertEquals(0.0, center.y, 0.01)
        Assert.assertEquals(0.0, center.z, 0.01)

        Assert.assertEquals(1.0, box.sizeY, 0.01)
        Assert.assertEquals(1.0, box.sizeZ, 0.01)
        Assert.assertEquals(1.0, box.sizeX, 0.01)
    }

    @Test
    @Throws(Exception::class)
    fun computeSceneMultipleCubeInComposite() {
        // This should produce multiple, non-overlapping cubes.
        for (rep in 0..49) {
            val cube = PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube")

            val composite = CompositeModel("Cube container.")

            val NUM_CUBES = 10

            for (i in 0..NUM_CUBES - 1) {
                composite.addComponentForModel(cube)
            }

            val converter = AbstractToConcrete(DefaultMeshFactory())

            val result = converter.computeScene(composite)

            Assert.assertEquals(0, result.getRootSceneNode().getDrawables().size.toLong())
            Assert.assertEquals(NUM_CUBES.toLong(), result.getRootSceneNode().getChildren().size.toLong())

            for (i in 0..NUM_CUBES - 1) {
                for (j in 0..NUM_CUBES - 1) {
                    if (i != j) {

                        val a = result.getRootSceneNode().getChildren()[i].getDrawables()[0].worldAABB
                        val b = result.getRootSceneNode().getChildren()[j].getDrawables()[0].worldAABB

                        Assert.assertFalse(a.intersects(b, 0.0))
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
            val cube = PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube")

            val composite = CompositeModel("Cube container.")

            // Create two cube components.
            val cubeA = composite.addComponentForModel(cube)
            val cubeB = composite.addComponentForModel(cube)
            val cubeC = composite.addComponentForModel(cube)

            composite.addConstraint(RelativePositionConstraint(cubeA, cubeB, ABOVE, Distance.ANY))
            composite.addConstraint(RelativePositionConstraint(cubeB, cubeC, ABOVE, Distance.ANY))

            // Compute the concrete scene
            val converter = AbstractToConcrete(DefaultMeshFactory())

            val scene = converter.computeScene(composite)

            // Should be three children
            val children = scene.getRootSceneNode().getChildren()
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
            val totalBox = scene.getRootSceneNode().computeWorldAABB()
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
            val cube = PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube")

            val composite = CompositeModel("Cube container.")

            // Create two cube components.
            val cubeA = composite.addComponentForModel(cube)
            val cubeB = composite.addComponentForModel(cube)
            val cubeC = composite.addComponentForModel(cube)

            composite.addConstraint(RelativePositionConstraint(cubeA, cubeB, ABOVE, FixedDistance(1.0)))
            composite.constraints.add(RelativePositionConstraint(cubeB, cubeC, ABOVE, FixedDistance(2.0)))

            // Compute the concrete scene
            val converter = AbstractToConcrete(DefaultMeshFactory())

            val scene = converter.computeScene(composite)

            // Should be three children
            val children = scene.getRootSceneNode().getChildren()
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
            val totalBox = scene.getRootSceneNode().computeWorldAABB()
            Assert.assertEquals(6.0, totalBox.sizeY, 0.5)
            Assert.assertEquals(1.0, totalBox.sizeX, 0.1)
            Assert.assertEquals(1.0, totalBox.sizeZ, 0.1)
        }
    }

}