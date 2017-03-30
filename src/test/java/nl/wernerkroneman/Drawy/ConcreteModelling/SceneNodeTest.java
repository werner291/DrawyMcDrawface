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

package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Vector3d;
import org.junit.Assert;
import org.junit.Test;

public class SceneNodeTest {
    @Test
    public void computeLocalAABB() throws Exception {

        MeshFactory meshFactory = new DefaultMeshFactory();
        PrimitiveGenerator primitiveGenerator = new PrimitiveGenerator(meshFactory);

        SceneNode root = new SceneNode();

        SceneNode child = new SceneNode();
        root.addChild(child);

        child.addDrawable(new Drawable(primitiveGenerator.generateUnitCube()));

        // Translation shouldn't affect, localAAB is in local coordinates
        child.setTranslation(new Vector3d(5, 9, 1));

        AABB box = child.computeLocalAABB();

        Assert.assertEquals(-0.5, box.getMinExtent().x, 0.01);
        Assert.assertEquals(-0.5, box.getMinExtent().y, 0.01);
        Assert.assertEquals(-0.5, box.getMinExtent().z, 0.01);

        Assert.assertEquals(0.5, box.getMaxExtent().x, 0.01);
        Assert.assertEquals(0.5, box.getMaxExtent().y, 0.01);
        Assert.assertEquals(0.5, box.getMaxExtent().z, 0.01);

        AABB rootBox = root.computeLocalAABB();

        Assert.assertEquals(4.5, rootBox.getMinExtent().x, 0.01);
        Assert.assertEquals(8.5, rootBox.getMinExtent().y, 0.01);
        Assert.assertEquals(0.5, rootBox.getMinExtent().z, 0.01);

        Assert.assertEquals(5.5, rootBox.getMaxExtent().x, 0.01);
        Assert.assertEquals(9.5, rootBox.getMaxExtent().y, 0.01);
        Assert.assertEquals(1.5, rootBox.getMaxExtent().z, 0.01);

    }

    @Test
    public void computeWorldAABB() throws Exception {
        MeshFactory meshFactory = new DefaultMeshFactory();
        PrimitiveGenerator primitiveGenerator = new PrimitiveGenerator(meshFactory);

        SceneNode root = new SceneNode();

        SceneNode child = new SceneNode();
        root.addChild(child);

        child.addDrawable(new Drawable(primitiveGenerator.generateUnitCube()));

        // Translation shouldn't affect, localAAB is in local coordinates
        child.setTranslation(new Vector3d(5, 9, 1));

        AABB box = child.computeWorldAABB();

        Assert.assertEquals(4.5, box.getMinExtent().x, 0.01);
        Assert.assertEquals(8.5, box.getMinExtent().y, 0.01);
        Assert.assertEquals(0.5, box.getMinExtent().z, 0.01);

        Assert.assertEquals(5.5, box.getMaxExtent().x, 0.01);
        Assert.assertEquals(9.5, box.getMaxExtent().y, 0.01);
        Assert.assertEquals(1.5, box.getMaxExtent().z, 0.01);
    }

}