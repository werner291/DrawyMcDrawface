package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Matrix4d;
import org.joml.Vector3d;

public class AABB {

    Vector3d maxExtent = new Vector3d();
    Vector3d minExtent = new Vector3d();

    /**
     * Initialize with 0-extent.
     */
    public AABB() {
    }

    /**
     * Initialize
     *
     * @param maxExtent
     * @param minExtent
     */
    public AABB(Vector3d maxExtent, Vector3d minExtent) {
        this.maxExtent.set(maxExtent);
        this.minExtent.set(minExtent);
    }

    /**
     * Extend the box such that it at least contains the vector.
     *
     * @param vec The vector to be covered.
     */
    public void extendToCover(Vector3d vec) {
        maxExtent.x = Math.max(maxExtent.x, vec.x);
        maxExtent.y = Math.max(maxExtent.y, vec.y);
        maxExtent.z = Math.max(maxExtent.z, vec.z);

        minExtent.x = Math.min(minExtent.x, vec.x);
        minExtent.y = Math.min(minExtent.y, vec.y);
        minExtent.z = Math.min(minExtent.z, vec.z);
    }

    /**
     * Compute the center of the box.
     *
     * @param dest Where to store the result (this is also returned)
     * @return The center
     */
    public Vector3d getCenter(Vector3d dest) {
        return maxExtent.add(minExtent, dest).div(2);
    }

    public double getHeight() {
        return maxExtent.y - minExtent.y;
    }

    public double getWidth() {
        return maxExtent.x - minExtent.x;
    }

    public double getDepth() {
        return maxExtent.z - minExtent.z;
    }

    /**
     * Compute the bounding box of a transformed version of this AABB
     * <p>
     * Note that this might yield a bounding box that is a lot bigger than
     * the original shape.
     * <p>
     * A more accurate (but more expensive) way would be to transform the shape itself
     * and then compute its' bounding box.
     *
     * @param mat  The transformation
     * @param dest The AAB to store the result in.
     * @return The bounding box.
     */
    public AABB transform(Matrix4d mat, AABB dest) {

        assert (dest != this); // Let's not go too crazy, shall we?

        dest.minExtent.set(Double.POSITIVE_INFINITY);
        dest.maxExtent.set(Double.NEGATIVE_INFINITY);

        dest.extendToCover(minExtent.mulPosition(mat));
        dest.extendToCover(maxExtent.mulPosition(mat));

        return dest;
    }

    /**
     * Test whether any point inside this AABB is also inside the other AABB.
     * Also returns true if the boxes just touch.
     *
     * @param b The other
     * @return Whether there is an intersection.
     */
    public boolean intersects(AABB b) {
        return !(minExtent.x > b.maxExtent.x ||
                minExtent.y > b.maxExtent.y ||
                minExtent.z > b.maxExtent.z ||
                b.minExtent.x > maxExtent.x ||
                b.minExtent.y > maxExtent.y ||
                b.minExtent.z > maxExtent.z);
    }

    /**
     * Test whether the given position is insde the box
     *
     * @param pos The position to test
     * @return Whether the position is inside (or on the boundary)
     */
    public boolean inside(Vector3d pos) {
        return minExtent.x <= pos.x && pos.x <= maxExtent.x &&
                minExtent.y <= pos.y && pos.y <= maxExtent.y &&
                minExtent.z <= pos.z && pos.z <= maxExtent.z;
    }

    public AABB translate(Vector3d translation, AABB aabb) {
        minExtent.add(translation, aabb.minExtent);
        maxExtent.add(translation, aabb.maxExtent);

        return aabb;
    }
}
