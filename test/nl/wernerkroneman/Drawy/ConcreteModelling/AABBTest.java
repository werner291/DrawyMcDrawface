package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Matrix4d;
import org.joml.Vector3d;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created by werner on 28-12-16.
 */
public class AABBTest {

    @Test
    public void inside() throws Exception {
        AABB aabb = new AABB(new Vector3d(1), new Vector3d(-1));

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

        AABB aabb = new AABB(new Vector3d(1), new Vector3d(-1));

        Vector3d pos = new Vector3d(1.1, 0, 0);
        Assert.assertFalse(aabb.inside(pos));

        aabb.extendToCover(pos);

        Assert.assertTrue(aabb.inside(pos));

        // --------

        pos = new Vector3d(0, -5, -33);
        Assert.assertFalse(aabb.inside(pos));

        aabb.extendToCover(pos);

        Assert.assertTrue(aabb.inside(pos));
    }

    @Test
    public void getCenter() throws Exception {
        AABB aabb = new AABB(new Vector3d(42, 11, -18), new Vector3d(22, -1, -19));

        Assert.assertEquals(aabb.getCenter(new Vector3d()).x, 32, 0.001);
        Assert.assertEquals(aabb.getCenter(new Vector3d()).y, 5, 0.001);
        Assert.assertEquals(aabb.getCenter(new Vector3d()).z, -18.5, 0.001);
    }

    @Test
    public void transform() throws Exception {
        AABB aabb = new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9));

        Matrix4d mat = new Matrix4d().identity().translate(-9, -8, 50);

        AABB result = aabb.transform(mat, new AABB());

        Assert.assertEquals(1, aabb.maxExtent.x, 0.001);
        Assert.assertEquals(-2, aabb.maxExtent.y, 0.001);
        Assert.assertEquals(60, aabb.maxExtent.z, 0.001);

        Assert.assertEquals(-9, aabb.minExtent.x, 0.001);
        Assert.assertEquals(-3, aabb.minExtent.y, 0.001);
        Assert.assertEquals(59, aabb.minExtent.z, 0.001);

    }

    @Test
    public void intersects() throws Exception {

        Assert.assertTrue((new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9)))
                .intersects((new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9)))));

        Assert.assertTrue((new AABB(new Vector3d(11, 7, 11), new Vector3d(-1, 4, 8)))
                .intersects((new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9)))));

        Assert.assertTrue(new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9))
                .intersects(new AABB(new Vector3d(11, 7, 11), new Vector3d(-1, 4, 8))));

        Assert.assertFalse(new AABB(new Vector3d(-1), new Vector3d(1))
                .intersects(new AABB(new Vector3d(-2), new Vector3d(-1))));

    }

    @Test
    public void translate() throws Exception {
        AABB box = new AABB(new Vector3d(10, 6, 10), new Vector3d(0, 5, 9));

        AABB result = box.translate(new Vector3d(5, 1, -50), new AABB());

        Assert.assertEquals(15, result.maxExtent.x, 0.01);
        Assert.assertEquals(7, result.maxExtent.y, 0.01);
        Assert.assertEquals(-40, result.maxExtent.z, 0.01);

        Assert.assertEquals(5, result.minExtent.x, 0.01);
        Assert.assertEquals(6, result.minExtent.y, 0.01);
        Assert.assertEquals(-41, result.minExtent.z, 0.01);
    }

}