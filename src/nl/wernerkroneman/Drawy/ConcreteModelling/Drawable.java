package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Matrix4d;

/**
 * Represents an instance of a Mesh in the scene.
 * Can be attached to a SceneNode.
 */
public class Drawable {

    Mesh mesh;

    SceneNode attached;

    public Drawable(Mesh mesh) {
        this.mesh = mesh;
    }

    public SceneNode getAttached() {
        return attached;
    }

    /**
     * Change the reference that the Drawable has to its' parent.
     * Must not already have a parent, set to null to change parent.
     *
     * @param attached The SceneNode to which this one is attached.
     */
    void _notifyAttached(SceneNode attached) {

        assert attached != null || attached == null;

        this.attached = attached;
    }

    public Mesh getMesh() {
        return mesh;
    }

    AABB getWorldAABB() {
        Matrix4d mat = attached.computeWorldTransform();
        return computeAABB().transform(mat, new AABB());
    }

    public AABB computeAABB() {
        return mesh.computeAABB();
    }
}
