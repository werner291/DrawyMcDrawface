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

import org.joml.Matrix4d;
import org.joml.Vector3d;
import org.junit.Assert;
import org.junit.Test;

public class AABBTest {

    @Test
    public void inside() throws Exception {
        AABB aabb = new AABB(-1, -1, -1, 1, 1, 1);

        Assert.assertTrue(aabb.inside(new Vector3d(0, 0, 0)));
        Assert.assertTrue(aabb.inside(new Vector3d(1, 1, 1)));
        Assert.assertTrue(aabb.inside(new Vector3d(-1, -1, -1)));
        Assert.assertTrue(aabb.inside(new Vector3d(0.5, 0.5, 0.3)));

        Assert.assertFalse(aabb.inside(new Vector3d(1.1, 0, 0)));
        Assert.assertFalse(aabb.inside(new Vector3d(1.1, 1.1, 0)));
        Assert.assertFalse(aabb.inside(new Vector3d(1.1, 1.1, 1.1)));

        Assert.assertFalse(aabb.inside(new Vector3d(-1.1, 0, 0)));
        Assert.assertFalse(aabb.inside(new Vector3d(-1.1, -1.1, 0)));
        Assert.assertFalse(aabb.inside(new Vector3d(-1.1, -1.1, -1.1)));
    }

    @Test
    public void extendToCover() throws Exception {

        AABB aabb = new AABB(-1, -1, -1, 1, 1, 1);

        Vector3d pos = new Vector3d(1.1, 0, 0);
        Assert.assertFalse(aabb.inside(pos));

        aabb = aabb.extendToCover(pos);

        Assert.assertTrue(aabb.inside(pos));

        // --------

        pos = new Vector3d(0, -5, -33);
        Assert.assertFalse(aabb.inside(pos));

        aabb = aabb.extendToCover(pos);

        Assert.assertTrue(aabb.inside(pos));
    }

    @Test
    public void getCenter() throws Exception {
        AABB aabb = new AABB(22, -1, -19, 42, 11, -18);

        Assert.assertEquals(aabb.getCenter().x, 32, 0.001);
        Assert.assertEquals(aabb.getCenter().y, 5, 0.001);
        Assert.assertEquals(aabb.getCenter().z, -18.5, 0.001);
    }

    @Test
    public void transform() throws Exception {
        AABB aabb = new AABB(0, 5, 9, 10, 6, 10);

        Matrix4d mat = new Matrix4d().identity().translate(-9, -8, 50);

        AABB result = aabb.transform(mat);

        Assert.assertEquals(1, result.getXMax(), 0.001);
        Assert.assertEquals(-2, result.getYMax(), 0.001);
        Assert.assertEquals(60, result.getZMax(), 0.001);

        Assert.assertEquals(-9, result.getXMin(), 0.001);
        Assert.assertEquals(-3, result.getYMin(), 0.001);
        Assert.assertEquals(59, result.getZMin(), 0.001);

    }

    @Test
    public void intersects() throws Exception {

        Assert.assertTrue((new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9)))
                .intersects((new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9))), 0));

        Assert.assertTrue((new AABB(new Vector3d(11, 7, 11), new Vector3d(-1, 4, 8)))
                .intersects((new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9))), 0));

        Assert.assertTrue(new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9))
                .intersects(new AABB(new Vector3d(11, 7, 11), new Vector3d(-1, 4, 8)), 0));

        Assert.assertFalse(new AABB(new Vector3d(-1), new Vector3d(1))
                .intersects(new AABB(new Vector3d(-2), new Vector3d(-1)), 0));

    }

    @Test
    public void translate() throws Exception {
        AABB box = new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9));

        AABB result = box.translate(new Vector3d(5, 1, -50));

        Assert.assertEquals(15, result.getMaxExtent().x, 0.01);
        Assert.assertEquals(7, result.getMaxExtent().y, 0.01);
        Assert.assertEquals(-40, result.getMaxExtent().z, 0.01);

        Assert.assertEquals(5, result.getMinExtent().x, 0.01);
        Assert.assertEquals(6, result.getMinExtent().y, 0.01);
        Assert.assertEquals(-41, result.getMinExtent().z, 0.01);
    }

}