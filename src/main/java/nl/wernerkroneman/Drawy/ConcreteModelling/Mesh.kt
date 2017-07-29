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

import org.joml.Vector2d
import org.joml.Vector3d

import java.util.ArrayList

/**
 * Represents a non-indexed triangle mesh.
 *
 *
 * Optionally supports vertex coloring and texture coordinates.
 */
open class Mesh {

    protected val vertices: MutableList<Vector3d> = ArrayList()
    protected val normals: MutableList<Vector3d> = ArrayList()
    protected val texCoords: List<Vector2d> = ArrayList()
    protected var colors: List<Vector3d> = ArrayList()

    /**
     * Add a triangle with the specified vertices, computes normals automatically
     */
    fun addTriangle(a: Vector3d, b: Vector3d, c: Vector3d) {

        val normal = Vector3d(b).sub(a).cross(Vector3d(c).sub(a)).normalize()

        addTriangle(a, b, c, normal)
    }

    /**
     * Add a triangle and the specified surface normal
     */
    private fun addTriangle(a: Vector3d, b: Vector3d, c: Vector3d, normal: Vector3d) {

        addTriangle(a, b, c, normal, normal, normal)
    }

    /**
     * Add a triangle with the specified vertices and one normal for each vertex.
     */
    protected open fun addTriangle(a: Vector3d, b: Vector3d, c: Vector3d,
                                   aN: Vector3d, bN: Vector3d, cN: Vector3d) {
        vertices.add(a)
        vertices.add(b)
        vertices.add(c)

        normals.add(aN)
        normals.add(bN)
        normals.add(cN)
    }

    /**
     * Compute the axis-aligned bounding box that completely encompasses
     * the mesh.
     *
     *
     * Mesh must be non-empty.
     *
     *
     * The AABB returned is a new instance and modificatiuons
     * to it won't modify the one associated with this mesh.

     * @return the AABB
     * *
     * @pre `!vertices.isEmpty()`
     */
    fun computeAABB(): AABB {

        assert(!vertices.isEmpty())

        return vertices.fold(AABB_REVERSE_INFINITY,
                { acc, vert -> acc.extendToCover(vert) })
    }

}
