package nl.wernerkroneman.Drawy.AbstractToConcreteConverter;

import nl.wernerkroneman.Drawy.ConcreteModelling.AABB;
import nl.wernerkroneman.Drawy.Modelling.FixedDistance;
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint;
import org.joml.Vector3d;

/**
 * Created by werner on 5-2-17.
 */
public class AABBAllowedSpaceComputations {
    public static AABB computeAllowedAABBForAnyDistance(Vector3d size, AABB allowedSpace, AABB relatedBBounds, RelativePositionConstraint.RelativePosition pos) {
        AABB constraintRestrictedSpace = new AABB();

        for (int dim = 0; dim < 3; dim++) {
            switch (pos.rel[dim]) {
                case BEFORE:
                    constraintRestrictedSpace.maxExtent.setComponent(dim,
                            Math.min(allowedSpace.maxExtent.get(dim), relatedBBounds.minExtent.get(dim)));
                    break;
                case AFTER:
                    constraintRestrictedSpace.minExtent.setComponent(dim,
                            Math.max(allowedSpace.minExtent.get(dim), relatedBBounds.maxExtent.get(dim)));
                    break;
                case SAME:
                    constraintRestrictedSpace.maxExtent.setComponent(dim,
                            Math.min(allowedSpace.maxExtent.get(dim), relatedBBounds.maxExtent.get(dim) + size.get(dim)));
                    constraintRestrictedSpace.minExtent.setComponent(dim,
                            Math.max(allowedSpace.minExtent.get(dim), relatedBBounds.minExtent.get(dim) - size.get(dim)));
                    break;
            }
        }
        return constraintRestrictedSpace;
    }

    /**
     * Compute the allowed space for a single constraint given a size,
     * while contraining that the object must be in more or less direct
     * contact with the object.
     *
     * @param size  The size of the object to place
     * @param relatedBBounds The bounding box of the related object
     * @param distance The distance between the two closest points of the objects
     * @param pos The relative position (above,below, etc...)
     * @return The space in which the object is allowed to be.
     */
    public static AABB computeAllowedAABBForFixedDistance(Vector3d size,
                                                          AABB relatedBBounds,
                                                          FixedDistance distance,
                                                          RelativePositionConstraint.RelativePosition pos) {
        AABB constraintRestrictedSpace = new AABB();

        double dist = distance.distance;

        // Iterate over all dimensions (X,Y,Z)
        for (int dim = 0; dim < 3; dim++) {
            switch (pos.rel[dim]) {
                case BEFORE: // Extent has to end before the other object
                    constraintRestrictedSpace.maxExtent.setComponent(dim,
                            relatedBBounds.minExtent.get(dim) - dist);

                    constraintRestrictedSpace.minExtent.setComponent(dim,
                            constraintRestrictedSpace.maxExtent.get(dim) - (size.get(dim)+0.01));
                    break;
                case AFTER: // Extent has to begin after the other object
                    constraintRestrictedSpace.minExtent.setComponent(dim,
                            relatedBBounds.maxExtent.get(dim) + dist);

                    constraintRestrictedSpace.maxExtent.setComponent(dim,
                            constraintRestrictedSpace.minExtent.get(dim) + (size.get(dim)+0.01));
                    break;
                case SAME: // Extent has to overlap
                    constraintRestrictedSpace.maxExtent.setComponent(dim,
                            constraintRestrictedSpace.maxExtent.get(dim) + size.get(dim));

                    constraintRestrictedSpace.minExtent.setComponent(dim,
                            constraintRestrictedSpace.minExtent.get(dim) - size.get(dim));
                    break;
            }
        }
        return constraintRestrictedSpace;
    }
}
