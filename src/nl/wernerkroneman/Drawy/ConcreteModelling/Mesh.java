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

    protected List<Vector3d> vertices = new ArrayList<>();
    protected List<Vector3d> normals = new ArrayList<>();
    protected List<Vector3d> colors = new ArrayList<>();
    protected List<Vector2d> texCoords = new ArrayList<>();

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
    public void addTriangle(Vector3d a, Vector3d b, Vector3d c, Vector3d normal) {

        addTriangle(a, b, c, normal, normal, normal);
    }

    /**
     * Add a triangle with the specified vertices and one normal for each vertex.
     */
    public void addTriangle(Vector3d a, Vector3d b, Vector3d c,
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
     * @pre !vertices.isEmpty()
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
