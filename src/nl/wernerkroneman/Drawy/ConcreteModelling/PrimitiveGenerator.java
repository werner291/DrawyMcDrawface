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

        cube.addTriangle(new Vector3d(0.5f, -0.5f, 0.5f),
                new Vector3d(-0.5f, -0.5f, -0.5f),
                new Vector3d(0.5f, -0.5f, -0.5f));

        cube.addTriangle(new Vector3d(-0.5f, -0.5f, -0.5f),
                new Vector3d(-0.5f, 0.5f, 0.5f),
                new Vector3d(-0.5f, 0.5f, -0.5f));

        cube.addTriangle(new Vector3d(0.5f, 0.5f, -0.5f),
                new Vector3d(-0.5f, -0.5f, -0.5f),
                new Vector3d(-0.5f, 0.5f, -0.5f));

        cube.addTriangle(new Vector3d(0.5f, 0.5f, -0.5f),
                new Vector3d(-0.5f, -0.5f, -0.5f),
                new Vector3d(0.5f, -0.5f, -0.5f));

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

    public Mesh generateSphere(double radius, int vertSegments, int horSegments) {

        Mesh mesh = meshFactory.createMesh();

        for (int h = 0; h < horSegments; h++) {

            double upperLatitude = -Math.PI / 2 + Math.PI * (double) (h + 1) / (double) horSegments;
            double upperRingRadius = radius * Math.cos(upperLatitude);
            double upperRingHeight = radius * Math.sin(upperLatitude);

            double lowerLongitude = -Math.PI / 2 + Math.PI * (double) h / (double) horSegments;
            double lowerRingRadius = radius * Math.cos(lowerLongitude);
            double lowerRingHeight = radius * Math.sin(lowerLongitude);

            for (int v = 0; v < vertSegments; v++) {
                double firstLongitude = 2.0 * v * Math.PI / vertSegments;
                double firstX = Math.cos(firstLongitude);
                double firstZ = Math.sin(firstLongitude);
                double secondLongitude = 2.0 * (v + 1) * Math.PI / vertSegments;
                double secondX = Math.cos(secondLongitude);
                double secondZ = Math.sin(secondLongitude);

                if (h < vertSegments - 1) {
                    mesh.addTriangle(new Vector3d(secondX * lowerRingRadius, lowerRingHeight, secondZ *
                                    lowerRingRadius),

                            new Vector3d(firstX * lowerRingRadius, lowerRingHeight, firstZ * lowerRingRadius),
                            new Vector3d(firstX * upperRingRadius, upperRingHeight, firstZ * upperRingRadius));
                }

                if (h > 0) {
                    mesh.addTriangle(new Vector3d(secondX * lowerRingRadius, lowerRingHeight, secondZ *
                                    lowerRingRadius),

                            new Vector3d(firstX * upperRingRadius, upperRingHeight, firstZ * upperRingRadius),
                            new Vector3d(secondX * upperRingRadius, upperRingHeight, secondZ * upperRingRadius));
                }
            }
        }

        return mesh;
    }

}
