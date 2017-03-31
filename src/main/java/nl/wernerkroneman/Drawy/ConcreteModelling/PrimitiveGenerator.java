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

            double lowerLatitude = -Math.PI / 2 + Math.PI * (double) h / (double) horSegments;
            double lowerRingRadius = radius * Math.cos(lowerLatitude);
            double lowerRingHeight = radius * Math.sin(lowerLatitude);

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
