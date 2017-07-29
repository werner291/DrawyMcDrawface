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

import org.joml.Matrix4d
import org.joml.Vector3d
import org.junit.Assert
import org.junit.Test

class SceneNodeTest {
    @Test
    @Throws(Exception::class)
    fun computeLocalAABB() {

        val meshFactory = DefaultMeshFactory()
        val primitiveGenerator = PrimitiveGenerator(meshFactory)

        val cubeDrawable = Drawable(primitiveGenerator.generateUnitCube())

        val translation = Matrix4d().translate(5.0, 9.0, 1.0)

        val childNode = SceneNode(drawables = listOf(cubeDrawable))

        val root = SceneNode(children = listOf(childNode), transform = translation)

        Assert.assertEquals(-0.5, childNode.aabb.xMin, 0.01)
        Assert.assertEquals(-0.5, childNode.aabb.yMin, 0.01)
        Assert.assertEquals(-0.5, childNode.aabb.zMin, 0.01)

        Assert.assertEquals(0.5, childNode.aabb.xMax, 0.01)
        Assert.assertEquals(0.5, childNode.aabb.yMax, 0.01)
        Assert.assertEquals(0.5, childNode.aabb.zMax, 0.01)

        Assert.assertEquals(4.5, root.aabb.xMin, 0.01)
        Assert.assertEquals(8.5, root.aabb.yMin, 0.01)
        Assert.assertEquals(0.5, root.aabb.zMin, 0.01)

        Assert.assertEquals(5.5, root.aabb.xMax, 0.01)
        Assert.assertEquals(9.5, root.aabb.yMax, 0.01)
        Assert.assertEquals(1.5, root.aabb.zMax, 0.01)

    }

}