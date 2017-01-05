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

        Assert.assertEquals(-0.5, box.minExtent.x, 0.01);
        Assert.assertEquals(-0.5, box.minExtent.y, 0.01);
        Assert.assertEquals(-0.5, box.minExtent.z, 0.01);

        Assert.assertEquals(0.5, box.maxExtent.x, 0.01);
        Assert.assertEquals(0.5, box.maxExtent.y, 0.01);
        Assert.assertEquals(0.5, box.maxExtent.z, 0.01);

        AABB rootBox = root.computeLocalAABB();

        Assert.assertEquals(4.5, rootBox.minExtent.x, 0.01);
        Assert.assertEquals(8.5, rootBox.minExtent.y, 0.01);
        Assert.assertEquals(0.5, rootBox.minExtent.z, 0.01);

        Assert.assertEquals(5.5, rootBox.maxExtent.x, 0.01);
        Assert.assertEquals(9.5, rootBox.maxExtent.y, 0.01);
        Assert.assertEquals(1.5, rootBox.maxExtent.z, 0.01);

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

        Assert.assertEquals(4.5, box.minExtent.x, 0.01);
        Assert.assertEquals(8.5, box.minExtent.y, 0.01);
        Assert.assertEquals(0.5, box.minExtent.z, 0.01);

        Assert.assertEquals(5.5, box.maxExtent.x, 0.01);
        Assert.assertEquals(9.5, box.maxExtent.y, 0.01);
        Assert.assertEquals(1.5, box.maxExtent.z, 0.01);
    }

}