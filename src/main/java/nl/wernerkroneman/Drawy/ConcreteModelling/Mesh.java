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

import org.joml.Vector2d;
import org.joml.Vector3d;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a non-indexed triangle mesh.
 * <p>
 * Optionally supports vertex coloring and texture coordinates.
 */
public class Mesh {

    protected final List<Vector3d> vertices = new ArrayList<>();
    protected final List<Vector3d> normals = new ArrayList<>();
    protected final List<Vector2d> texCoords = new ArrayList<>();
    protected List<Vector3d> colors = new ArrayList<>();

    /**
     * Add a triangle with the specified vertices, computes normals automatically
     */
    public void addTriangle(Vector3d a, Vector3d b, Vector3d c) {

        Vector3d normal = new Vector3d(b).sub(a).cross(new Vector3d(c).sub(a)).normalize();

        addTriangle(a, b, c, normal);
    }

    /**
     * Add a triangle and the specified surface normal
     */
    private void addTriangle(Vector3d a, Vector3d b, Vector3d c, Vector3d normal) {

        addTriangle(a, b, c, normal, normal, normal);
    }

    /**
     * Add a triangle with the specified vertices and one normal for each vertex.
     */
    protected void addTriangle(Vector3d a, Vector3d b, Vector3d c,
                               Vector3d aN, Vector3d bN, Vector3d cN) {
        vertices.add(a);
        vertices.add(b);
        vertices.add(c);

        normals.add(aN);
        normals.add(bN);
        normals.add(cN);
    }

    /**
     * Compute the axis-aligned bounding box that completely encompasses
     * the mesh.
     * <p>
     * Mesh must be non-empty.
     * <p>
     * The AABB returned is a new instance and modificatiuons
     * to it won't modify the one associated with this mesh.
     *
     * @return the AABB
     * @pre {@code !vertices.isEmpty()}
     */
    public AABB computeAABB() {

        assert (!vertices.isEmpty());

        AABB box = new AABB(new Vector3d(Double.NEGATIVE_INFINITY), new Vector3d(Double.POSITIVE_INFINITY));

        for (Vector3d vert : vertices) {
            box.extendToCover(vert);
        }

        return box;
    }

}
