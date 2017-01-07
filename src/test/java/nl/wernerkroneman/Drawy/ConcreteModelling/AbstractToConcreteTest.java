package nl.wernerkroneman.Drawy.ConcreteModelling;

import nl.wernerkroneman.Drawy.AbstractToConcreteConverter.AbstractToConcrete;
import nl.wernerkroneman.Drawy.AbstractToConcreteConverter.PositionalSolver;
import nl.wernerkroneman.Drawy.Modelling.*;
import org.joml.Vector3d;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.*;

/**
 * Created by werner on 27-12-16.
 */
public class AbstractToConcreteTest {
    @Test
    public void getTranslationToFit() throws Exception {

        MeshFactory meshFactory = new DefaultMeshFactory();
        PrimitiveGenerator primitiveGenerator = new PrimitiveGenerator(meshFactory);

        SceneNode root = new SceneNode();

        SceneNode child = new SceneNode();
        root.addChild(child);

        child.addDrawable(new Drawable(primitiveGenerator.generateUnitCube()));

        AbstractToConcrete converter = new AbstractToConcrete(meshFactory);

        Vector3d trans = converter.getTranslationToFit(child, child.computeLocalAABB());
        Assert.assertEquals(0, trans.length(), 0.01);

        Vector3d trans2 = converter.getTranslationToFit(child, new AABB(new Vector3d(-50), new Vector3d(-51)));
        Assert.assertEquals(-50.5, trans2.x, 0.01);
        Assert.assertEquals(-50.5, trans2.y, 0.01);
        Assert.assertEquals(-50.5, trans2.z, 0.01);

    }

    @Test
    public void findEmptyPlaceTest() throws Exception {

        MeshFactory meshFactory = new DefaultMeshFactory();
        PrimitiveGenerator primitiveGenerator = new PrimitiveGenerator(meshFactory);

        for (int i = 0;
             i < 100;
             i++) {
            SceneNode root = new SceneNode();

            SceneNode child = new SceneNode();
            root.addChild(child);

            child.addDrawable(new Drawable(primitiveGenerator.generateUnitCube()));

            AbstractToConcrete converter = new AbstractToConcrete(meshFactory);

            List<AABB> doesNotIntersect = new ArrayList<>();
            doesNotIntersect.add(child.computeWorldAABB());

            AABB empty = PositionalSolver.findEmptyPlace(doesNotIntersect, 0, new Vector3d(2),
                    new AABB(new Vector3d(Double.POSITIVE_INFINITY),
                             new Vector3d(Double.NEGATIVE_INFINITY)));

            Assert.assertFalse(empty.intersects(new AABB(new Vector3d(0.5), new Vector3d(-0.5)), 0));

            Assert.assertEquals(2, empty.getSizeZ(), 0.01);
            Assert.assertEquals(2, empty.getSizeX(), 0.01);
            Assert.assertEquals(2, empty.getSizeY(), 0.01);

            Assert.assertTrue(empty.getCenter(new Vector3d()).length() < 10);

        }
    }

    @Test
    public void computeSceneSingleCube() throws Exception {
        // This should produce a single cube centered at (0,0,0) of edge length 1.

        PrimitiveModel cube = new PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube");

        AbstractToConcrete converter = new AbstractToConcrete(new DefaultMeshFactory());

        Scene result = converter.computeScene(cube);

        Assert.assertEquals(1, result.getRootSceneNode().getDrawables().size());

        Mesh cubemesh = result.getRootSceneNode().getDrawables().get(0).getMesh();

        AABB box = cubemesh.computeAABB();

        Vector3d center = box.getCenter(new Vector3d());
        Assert.assertEquals(0, center.x, 0.01);
        Assert.assertEquals(0, center.y, 0.01);
        Assert.assertEquals(0, center.z, 0.01);

        Assert.assertEquals(1, box.getSizeY(), 0.01);
        Assert.assertEquals(1, box.getSizeZ(), 0.01);
        Assert.assertEquals(1, box.getSizeX(), 0.01);
    }

    @Test
    public void computeSceneSingleCubeInComposite() throws Exception {
        // This should produce a single cube centered at (0,0,0) of edge length 1.

        PrimitiveModel cube = new PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube");

        CompositeModel composite = new CompositeModel("Cube container.");

        composite.addComponentForModel(cube);

        AbstractToConcrete converter = new AbstractToConcrete(new DefaultMeshFactory());

        Scene result = converter.computeScene(composite);

        Assert.assertEquals(0, result.getRootSceneNode().getDrawables().size());
        Assert.assertEquals(1, result.getRootSceneNode().getChildren().size());

        SceneNode childNode = result.getRootSceneNode().getChildren().get(0);
        Assert.assertEquals(1, childNode.getDrawables().size());

        Mesh cubemesh = childNode.getDrawables().get(0).getMesh();

        AABB box = cubemesh.computeAABB();

        Vector3d center = box.getCenter(new Vector3d());
        Assert.assertEquals(0, center.x, 0.01);
        Assert.assertEquals(0, center.y, 0.01);
        Assert.assertEquals(0, center.z, 0.01);

        Assert.assertEquals(1, box.getSizeY(), 0.01);
        Assert.assertEquals(1, box.getSizeZ(), 0.01);
        Assert.assertEquals(1, box.getSizeX(), 0.01);
    }

    @Test
    public void computeSceneMultipleCubeInComposite() throws Exception {
        // This should produce multiple, non-overlapping cubes.
        for (int rep = 0; rep < 50; rep++) {
            PrimitiveModel cube = new PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube");

            CompositeModel composite = new CompositeModel("Cube container.");

            final int NUM_CUBES = 10;

            for (int i = 0; i < NUM_CUBES; i++) {
                composite.addComponentForModel(cube);
            }

            AbstractToConcrete converter = new AbstractToConcrete(new DefaultMeshFactory());

            Scene result = converter.computeScene(composite);

            Assert.assertEquals(0, result.getRootSceneNode().getDrawables().size());
            Assert.assertEquals(NUM_CUBES, result.getRootSceneNode().getChildren().size());

            for (int i = 0;
                 i < NUM_CUBES;
                 i++) {
                for (int j = 0;
                     j < NUM_CUBES;
                     j++) {
                    if (i != j) {

                        AABB a = result.getRootSceneNode().getChildren().get(i).getDrawables().get(0).getWorldAABB();
                        AABB b = result.getRootSceneNode().getChildren().get(j).getDrawables().get(0).getWorldAABB();

                        Assert.assertFalse(a.intersects(b, 0));
                    }
                }
            }
        }
    }

    /**
     * Supposed to create a stack of 3 cubes.
     */
    @Test
    public void testRelativePositionConstraint() {
        for (int rep = 0; rep < 100; rep++) {
            // Set up a simple scene with two cubes
            PrimitiveModel cube = new PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube");

            CompositeModel composite = new CompositeModel("Cube container.");

            // Create two cube components.
            CompositeModel.Component cubeA = composite.addComponentForModel(cube);
            CompositeModel.Component cubeB = composite.addComponentForModel(cube);
            CompositeModel.Component cubeC = composite.addComponentForModel(cube);

            composite.getConstraints().add(new RelativePositionConstraint(cubeA, cubeB, ABOVE, Distance.ANY));
            composite.getConstraints().add(new RelativePositionConstraint(cubeB, cubeC, ABOVE, Distance.ANY));

            // Compute the concrete scene
            AbstractToConcrete converter = new AbstractToConcrete(new DefaultMeshFactory());

            Scene scene = converter.computeScene(composite);

            // Should be three children
            List<SceneNode> children = scene.getRootSceneNode().getChildren();
            Assert.assertEquals(3, children.size());

            // Check for intersecting pairs
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    if (i != j) {
                        AABB boxA = children.get(i).computeWorldAABB();
                        AABB boxB = children.get(j).computeWorldAABB();
                        Assert.assertFalse(boxA.intersects(boxB, 0.01));
                    }
                }
            }

            // Check whether the bounding box is consistent with a stack of 3 cubes.
            AABB totalBox = scene.getRootSceneNode().computeWorldAABB();
            Assert.assertEquals(3, totalBox.getSizeY(), 0.5);
            Assert.assertEquals(1, totalBox.getSizeX(), 0.1);
            Assert.assertEquals(1, totalBox.getSizeZ(), 0.1);
        }
    }

    /**
     * Supposed to create a stack of 3 cubes.
     */
    @Test
    public void testRelativePositionConstraintExactDistance() {
        for (int rep = 0; rep < 50; rep++) {
            // Set up a simple scene with two cubes
            PrimitiveModel cube = new PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube");

            CompositeModel composite = new CompositeModel("Cube container.");

            // Create two cube components.
            CompositeModel.Component cubeA = composite.addComponentForModel(cube);
            CompositeModel.Component cubeB = composite.addComponentForModel(cube);
            CompositeModel.Component cubeC = composite.addComponentForModel(cube);

            composite.getConstraints().add(new RelativePositionConstraint(cubeA, cubeB, ABOVE, new FixedDistance(1)));
            composite.getConstraints().add(new RelativePositionConstraint(cubeB, cubeC, ABOVE, new FixedDistance(2)));

            // Compute the concrete scene
            AbstractToConcrete converter = new AbstractToConcrete(new DefaultMeshFactory());

            Scene scene = converter.computeScene(composite);

            // Should be three children
            List<SceneNode> children = scene.getRootSceneNode().getChildren();
            Assert.assertEquals(3, children.size());

            // Check for intersecting pairs
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    if (i != j) {
                        AABB boxA = children.get(i).computeWorldAABB();
                        AABB boxB = children.get(j).computeWorldAABB();
                        Assert.assertFalse(boxA.intersects(boxB, 0.01));
                    }
                }
            }

            // Check whether the bounding box is consistent with a stack of 3 cubes.
            AABB totalBox = scene.getRootSceneNode().computeWorldAABB();
            Assert.assertEquals(6, totalBox.getSizeY(), 0.5);
            Assert.assertEquals(1, totalBox.getSizeX(), 0.1);
            Assert.assertEquals(1, totalBox.getSizeZ(), 0.1);
        }
    }

}