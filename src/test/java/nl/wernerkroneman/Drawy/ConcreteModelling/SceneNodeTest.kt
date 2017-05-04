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

import org.joml.Vector3d
import org.junit.Assert
import org.junit.Test

class SceneNodeTest {
    @Test
    @Throws(Exception::class)
    fun computeLocalAABB() {

        val meshFactory = DefaultMeshFactory()
        val primitiveGenerator = PrimitiveGenerator(meshFactory)

        val root = SceneNode()

        val child = SceneNode()
        root.addChild(child)

        child.addDrawable(Drawable(primitiveGenerator.generateUnitCube()))

        // Translation shouldn't affect, localAAB is in local coordinates
        child.setTranslation(Vector3d(5.0, 9.0, 1.0))

        val box = child.computeLocalAABB()

        Assert.assertEquals(-0.5, box.minExtent.x, 0.01)
        Assert.assertEquals(-0.5, box.minExtent.y, 0.01)
        Assert.assertEquals(-0.5, box.minExtent.z, 0.01)

        Assert.assertEquals(0.5, box.maxExtent.x, 0.01)
        Assert.assertEquals(0.5, box.maxExtent.y, 0.01)
        Assert.assertEquals(0.5, box.maxExtent.z, 0.01)

        val rootBox = root.computeLocalAABB()

        Assert.assertEquals(4.5, rootBox.minExtent.x, 0.01)
        Assert.assertEquals(8.5, rootBox.minExtent.y, 0.01)
        Assert.assertEquals(0.5, rootBox.minExtent.z, 0.01)

        Assert.assertEquals(5.5, rootBox.maxExtent.x, 0.01)
        Assert.assertEquals(9.5, rootBox.maxExtent.y, 0.01)
        Assert.assertEquals(1.5, rootBox.maxExtent.z, 0.01)

    }

    @Test
    @Throws(Exception::class)
    fun computeWorldAABB() {
        val meshFactory = DefaultMeshFactory()
        val primitiveGenerator = PrimitiveGenerator(meshFactory)

        val root = SceneNode()

        val child = SceneNode()
        root.addChild(child)

        child.addDrawable(Drawable(primitiveGenerator.generateUnitCube()))

        // Translation shouldn't affect, localAAB is in local coordinates
        child.setTranslation(Vector3d(5.0, 9.0, 1.0))

        val box = child.computeWorldAABB()

        Assert.assertEquals(4.5, box.minExtent.x, 0.01)
        Assert.assertEquals(8.5, box.minExtent.y, 0.01)
        Assert.assertEquals(0.5, box.minExtent.z, 0.01)

        Assert.assertEquals(5.5, box.maxExtent.x, 0.01)
        Assert.assertEquals(9.5, box.maxExtent.y, 0.01)
        Assert.assertEquals(1.5, box.maxExtent.z, 0.01)
    }

}