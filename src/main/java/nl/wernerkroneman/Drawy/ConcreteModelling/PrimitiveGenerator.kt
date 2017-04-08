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

class PrimitiveGenerator(internal var meshFactory: MeshFactory) {

    /**
     * Generate a simple cube, it will be centered on (0,0,0),
     * have edges 1 unit in length and have normals.

     * @return A Mesh representing a cube
     */
    fun generateUnitCube(): Mesh {

        val cube = meshFactory.createMesh()

        // Vertex coordinates are hard-coded, normals are computed automatically

        cube.addTriangle(Vector3d(-0.5, -0.5, -0.5),
                Vector3d(-0.5, -0.5, 0.5),
                Vector3d(-0.5, 0.5, 0.5))

        cube.addTriangle(Vector3d(0.5, -0.5, 0.5),
                Vector3d(-0.5, -0.5, -0.5),
                Vector3d(0.5, -0.5, -0.5))

        cube.addTriangle(Vector3d(-0.5, -0.5, -0.5),
                Vector3d(-0.5, 0.5, 0.5),
                Vector3d(-0.5, 0.5, -0.5))

        cube.addTriangle(Vector3d(0.5, 0.5, -0.5),
                Vector3d(-0.5, -0.5, -0.5),
                Vector3d(-0.5, 0.5, -0.5))

        cube.addTriangle(Vector3d(0.5, 0.5, -0.5),
                Vector3d(-0.5, -0.5, -0.5),
                Vector3d(0.5, -0.5, -0.5))

        cube.addTriangle(Vector3d(0.5, -0.5, 0.5),
                Vector3d(-0.5, -0.5, 0.5),
                Vector3d(-0.5, -0.5, -0.5))

        cube.addTriangle(Vector3d(-0.5, 0.5, 0.5),
                Vector3d(-0.5, -0.5, 0.5),
                Vector3d(0.5, -0.5, 0.5))

        cube.addTriangle(Vector3d(0.5, 0.5, 0.5),
                Vector3d(0.5, -0.5, -0.5),
                Vector3d(0.5, 0.5, -0.5))

        cube.addTriangle(Vector3d(0.5, -0.5, -0.5),
                Vector3d(0.5, 0.5, 0.5),
                Vector3d(0.5, -0.5, 0.5))

        cube.addTriangle(Vector3d(0.5, 0.5, 0.5),
                Vector3d(0.5, 0.5, -0.5),
                Vector3d(-0.5, 0.5, -0.5))

        cube.addTriangle(Vector3d(0.5, 0.5, 0.5),
                Vector3d(-0.5, 0.5, -0.5),
                Vector3d(-0.5, 0.5, 0.5))

        cube.addTriangle(Vector3d(0.5, 0.5, 0.5),
                Vector3d(-0.5, 0.5, 0.5),
                Vector3d(0.5, -0.5, 0.5))

        return cube
    }

    fun generateSphere(radius: Double, vertSegments: Int, horSegments: Int): Mesh {

        val mesh = meshFactory.createMesh()

        for (h in 0..horSegments - 1) {

            val upperLatitude = -Math.PI / 2 + Math.PI * (h + 1).toDouble() / horSegments.toDouble()
            val upperRingRadius = radius * Math.cos(upperLatitude)
            val upperRingHeight = radius * Math.sin(upperLatitude)

            val lowerLatitude = -Math.PI / 2 + Math.PI * h.toDouble() / horSegments.toDouble()
            val lowerRingRadius = radius * Math.cos(lowerLatitude)
            val lowerRingHeight = radius * Math.sin(lowerLatitude)

            for (v in 0..vertSegments - 1) {
                val firstLongitude = 2.0 * v.toDouble() * Math.PI / vertSegments
                val firstX = Math.cos(firstLongitude)
                val firstZ = Math.sin(firstLongitude)
                val secondLongitude = 2.0 * (v + 1).toDouble() * Math.PI / vertSegments
                val secondX = Math.cos(secondLongitude)
                val secondZ = Math.sin(secondLongitude)

                if (h < vertSegments - 1) {
                    mesh.addTriangle(Vector3d(secondX * lowerRingRadius, lowerRingHeight, secondZ * lowerRingRadius),

                            Vector3d(firstX * lowerRingRadius, lowerRingHeight, firstZ * lowerRingRadius),
                            Vector3d(firstX * upperRingRadius, upperRingHeight, firstZ * upperRingRadius))
                }

                if (h > 0) {
                    mesh.addTriangle(Vector3d(secondX * lowerRingRadius, lowerRingHeight, secondZ * lowerRingRadius),

                            Vector3d(firstX * upperRingRadius, upperRingHeight, firstZ * upperRingRadius),
                            Vector3d(secondX * upperRingRadius, upperRingHeight, secondZ * upperRingRadius))
                }
            }
        }

        return mesh
    }

    fun generateCylinder(radius: Double, height: Double, segments: Int): Mesh {
        val mesh = meshFactory.createMesh()

        val lowerPole = Vector3d(0.0, 0.0, 0.0)
        val upperPole = Vector3d(0.0, height, 0.0)

        for (i in 0..segments - 1) {

            val angleMin = 2.0 * i.toDouble() * Math.PI / segments.toDouble()
            val angleMax = 2.0 * (i + 1).toDouble() * Math.PI / segments.toDouble()

            val lowerA = Vector3d(Math.cos(angleMin) * radius, 0.0, Math.sin(angleMin))
            val lowerB = Vector3d(Math.cos(angleMax) * radius, 0.0, Math.sin(angleMax))
            val upperA = Vector3d(Math.cos(angleMin) * radius, height, Math.sin(angleMin))
            val upperB = Vector3d(Math.cos(angleMax) * radius, height, Math.sin(angleMax))

            mesh.addTriangle(lowerPole, lowerA, lowerB)
            mesh.addTriangle(lowerA, lowerB, upperA)
            mesh.addTriangle(lowerB, upperB, upperA)
            mesh.addTriangle(upperPole, upperA, upperB)

        }

        return mesh
    }
}
