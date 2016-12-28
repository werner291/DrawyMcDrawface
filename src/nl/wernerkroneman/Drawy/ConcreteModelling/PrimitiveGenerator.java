package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Vector3d;

public class PrimitiveGenerator {

    MeshFactory meshFactory;

    public PrimitiveGenerator(MeshFactory meshFactory) {
        this.meshFactory = meshFactory;
    }

    /**
     * Generate a simple cube, it will be centered on (0,0,0),
     * have edges 1 unit in length and have normals.
     *
     * @return A Mesh representing a cube
     */
    public Mesh generateUnitCube() {

        Mesh cube = meshFactory.createMesh();

        // Vertex coordinates are hard-coded, normals are computed automatically

        cube.addTriangle(new Vector3d(-0.5f, -0.5f, -0.5f),
                new Vector3d(-0.5f, -0.5f, 0.5f),
                new Vector3d(-0.5f, 0.5f, 0.5f));
        cube.addTriangle(new Vector3d(0.5f, 0.5f, -0.5f),
                new Vector3d(-0.5f, -0.5f, -0.5f),
                new Vector3d(-0.5f, 0.5f, -0.5f));
        cube.addTriangle(new Vector3d(0.5f, -0.5f, 0.5f),
                new Vector3d(-0.5f, -0.5f, -0.5f),
                new Vector3d(0.5f, -0.5f, -0.5f));
        cube.addTriangle(new Vector3d(0.5f, 0.5f, -0.5f),
                new Vector3d(0.5f, -0.5f, -0.5f),
                new Vector3d(-0.5f, -0.5f, -0.5f));
        cube.addTriangle(new Vector3d(-0.5f, -0.5f, -0.5f),
                new Vector3d(-0.5f, 0.5f, 0.5f),
                new Vector3d(-0.5f, 0.5f, -0.5f));
        cube.addTriangle(new Vector3d(0.5f, -0.5f, 0.5f),
                new Vector3d(-0.5f, -0.5f, 0.5f),
                new Vector3d(-0.5f, -0.5f, -0.5f));
        cube.addTriangle(new Vector3d(-0.5f, 0.5f, 0.5f),
                new Vector3d(-0.5f, -0.5f, 0.5f),
                new Vector3d(0.5f, -0.5f, 0.5f));
        cube.addTriangle(new Vector3d(0.5f, 0.5f, 0.5f),
                new Vector3d(0.5f, -0.5f, -0.5f),
                new Vector3d(0.5f, 0.5f, -0.5f));
        cube.addTriangle(new Vector3d(0.5f, -0.5f, -0.5f),
                new Vector3d(0.5f, 0.5f, 0.5f),
                new Vector3d(0.5f, -0.5f, 0.5f));
        cube.addTriangle(new Vector3d(0.5f, 0.5f, 0.5f),
                new Vector3d(0.5f, 0.5f, -0.5f),
                new Vector3d(-0.5f, 0.5f, -0.5f));
        cube.addTriangle(new Vector3d(0.5f, 0.5f, 0.5f),
                new Vector3d(-0.5f, 0.5f, -0.5f),
                new Vector3d(-0.5f, 0.5f, 0.5f));
        cube.addTriangle(new Vector3d(0.5f, 0.5f, 0.5f),
                new Vector3d(-0.5f, 0.5f, 0.5f),
                new Vector3d(0.5f, -0.5f, 0.5f));

        return cube;
    }

}
