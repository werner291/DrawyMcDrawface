package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Matrix4d;
import org.joml.Vector3d;

public class AABB {

    public Vector3d maxExtent = new Vector3d();
    public Vector3d minExtent = new Vector3d();

    /**
     * Initialize with 0-extent.
     */
    public AABB() {
    }

    /**
     * Initialize
     *
     * @param maxExtent The corner of the AABB with the highest x,y,z
     * @param minExtent The corner of the AABB with the loewst x,y,z
     */
    public AABB(Vector3d maxExtent, Vector3d minExtent) {
        this.maxExtent.set(maxExtent);
        this.minExtent.set(minExtent);
    }

    public AABB(AABB toCopy) {
        this.maxExtent.set(toCopy.maxExtent);
        this.minExtent.set(toCopy.minExtent);
    }

    /**
     * Get a point that can subjectively be considered the "most important point" in a range.
     *
     * For (-inf,int), that's 0.
     * For a half-bounded line, it's the bound.
     * For a completely bounded line, that's the middle.
     *
     * @param a The first bound
     * @param b The second bound
     * @return See description
     */
    private static double getExtentMean(double a, double b) {
        if (Double.isFinite(a)) {
            if (Double.isFinite(b)) {
                return (a + b) / 2.0;
            } else {
                return a;
            }
        } else {
            if (Double.isFinite(b)) {
                return b;
            } else {
                return 0;
            }
        }
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

    /**
     * Get a finite AABB inside this AABB that is roughly centered inside this one,
     * or sticking to an edge if half-infinite.
     * <p>
     * The parameters give a maximum size, but note tha tthe actual result may be much smaller.
     *
     * @param maxWidth  The maximum width of the AABB (x)
     * @param maxHeight The maximum height (y)
     * @param maxLength The maximum length of the AABB (z)
     * @param dest      Where to store the result
     * @return The result (same as dest)
     */
    public AABB getFiniteWithBounds(double maxWidth, double maxHeight, double maxLength, AABB dest) {

        Vector3d centeredOn = new Vector3d(getExtentMean(minExtent.x, maxExtent.x),
                getExtentMean(minExtent.y, maxExtent.y),
                getExtentMean(minExtent.z, maxExtent.z));

        centeredOn.add(maxWidth / 2, maxHeight / 2, maxLength / 2, dest.maxExtent);
        centeredOn.sub(maxWidth / 2, maxHeight / 2, maxLength / 2, dest.minExtent);

        return dest.intersection(this, dest);
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
     * @param tolerance How far into eachother the AABBs have to be before they "intersect"
     * @return Whether there is an intersection.
     */
    public boolean intersects(AABB b, double tolerance) {
        return !(minExtent.x + tolerance > b.maxExtent.x ||
                minExtent.y + tolerance > b.maxExtent.y ||
                minExtent.z + tolerance > b.maxExtent.z ||
                b.minExtent.x + tolerance > maxExtent.x ||
                b.minExtent.y + tolerance > maxExtent.y ||
                b.minExtent.z + tolerance > maxExtent.z);
    }

    /**
     * Computes the intersection of two AABBs
     *
     * @param other Which one to intersect this AABB with
     * @param dest  Where to store the result
     * @return Rturns dest after it has been modified
     * @modifies dest
     */
    public AABB intersection(AABB other, AABB dest) {

        dest.maxExtent.set(Math.min(other.maxExtent.x, maxExtent.x),
                Math.min(other.maxExtent.y, maxExtent.y),
                Math.min(other.maxExtent.z, maxExtent.z));

        dest.minExtent.set(Math.max(other.minExtent.x, minExtent.x),
                Math.max(other.minExtent.y, minExtent.y),
                Math.max(other.minExtent.z, minExtent.z));

        return dest;
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

    /**
     * Compute the translation of this AABB
     *
     * @param translation The translation
     * @param aabb        Where to store the result (will be overwritten)
     * @return aabb after is has been modified to represent the translation.
     */
    public AABB translate(Vector3d translation, AABB aabb) {
        minExtent.add(translation, aabb.minExtent);
        maxExtent.add(translation, aabb.maxExtent);

        return aabb;
    }
}
