package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Matrix4d;
import org.joml.Vector3d;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A node in the scene graph.
 */
public class SceneNode {

    /**
     * @inv parent != null implies parent.children.contains(this)
     * @inv forall child in children: child.parent == this
     */
    SceneNode parent;
    Matrix4d transform = new Matrix4d();
    private List<SceneNode> children = new ArrayList<>();
    private List<Drawable> drawables = new ArrayList<>();

    public SceneNode getParent() {
        return parent;
    }

    /**
     * Set the parent of this node in the tree.
     * <p>
     * Note: do not call directly, use the add/remove child
     * methods of the parent, as the parents need to be
     * updated as well to maintain a correct structure.
     *
     * @param parent
     */
    protected void setParent(SceneNode parent) {
        this.parent = parent;
    }

    /**
     * Get a list of drwable objects attached to this node.
     *
     * @return A list of drawable objects
     */
    public List<Drawable> getDrawables() {
        return Collections.unmodifiableList(drawables);
    }

    /**
     * Get a list of children.
     *
     * @return a list of children
     */
    public List<SceneNode> getChildren() {
        return Collections.unmodifiableList(children);
    }

    /**
     * Add a child node to this SceneNode.
     *
     * @param node The node to add, must not already have a parent.
     */
    public void addChild(SceneNode node) {
        assert node.getParent() == null;
        children.add(node);
        node.setParent(this);
    }

    /**
     * Return the full world transform, assuming the root node's transform
     * is a world transform, which is then multiplied down the tree.
     * <p>
     * This matrix is a new matrix and does not modify existing data.
     *
     * @return A transformation matrix
     */
    public Matrix4d computeWorldTransform() {
        if (parent != null) {
            return parent.computeWorldTransform().mul(this.transform);
        } else {
            return new Matrix4d(this.transform);
        }
    }

    public void addDrawable(Drawable drawable) {
        assert drawable.getAttached() == null;
        drawables.add(drawable);
        drawable._notifyAttached(this);
    }

    /**
     * Compute the AABB of the contents of this scene node
     * in this node's local coordinate frame.
     *
     * @return the AABB.
     */
    public AABB computeLocalAABB() {

        // Initialize the AABB as empty and negatively space-spanning
        // Any call to extendToCover will center the box on the position
        // and have volume 0.
        AABB result = new AABB(new Vector3d(Double.NEGATIVE_INFINITY),
                new Vector3d(Double.POSITIVE_INFINITY));

        for (SceneNode child : children) {
            // Compute the bounding box of the child, and transform
            // using the child's transform to bring it to this node's
            // local coordinate space.
            AABB childBox = child.computeLocalAABB().transform(child.getTransform(), new AABB());

            // Extend the result AABB to cover the child space.
            result.extendToCover(childBox.minExtent);
            result.extendToCover(childBox.maxExtent);
        }

        for (Drawable drawable : drawables) {
            // Get the AABB of the child.
            AABB childBox = drawable.computeAABB();

            result.extendToCover(childBox.minExtent);
            result.extendToCover(childBox.maxExtent);
        }

        return result;
    }

    public AABB computeWorldAABB() {
        return computeLocalAABB().transform(computeWorldTransform(), new AABB());
    }

    public Matrix4d getTransform() {
        return transform;
    }

    BfsIterator bfsIterator() {
        return new BfsIterator(this);
    }

    public void setTranslation(Vector3d sub) {
        transform.setTranslation(sub);
    }
}
