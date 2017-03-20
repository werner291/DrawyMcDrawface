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

package nl.wernerkroneman.Drawy.AbstractToConcreteConverter

import nl.wernerkroneman.Drawy.ConcreteModelling.AABB
import nl.wernerkroneman.Drawy.Modelling.FixedDistance
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint
import org.joml.Vector3d

/**
 * Created by werner on 5-2-17.
 */
object AABBAllowedSpaceComputations {
    fun computeAllowedAABBForAnyDistance(size: Vector3d, allowedSpace: AABB, relatedBBounds: AABB, pos: RelativePositionConstraint.RelativePosition): AABB {
        val constraintRestrictedSpace = AABB()

        for (dim in 0..2) {
            when (pos.rel[dim]) {
                RelativePositionConstraint.RelativePosition.DimensionOrder.BEFORE -> constraintRestrictedSpace.maxExtent.setComponent(dim,
                        Math.min(allowedSpace.maxExtent.get(dim), relatedBBounds.minExtent.get(dim)))
                RelativePositionConstraint.RelativePosition.DimensionOrder.AFTER -> constraintRestrictedSpace.minExtent.setComponent(dim,
                        Math.max(allowedSpace.minExtent.get(dim), relatedBBounds.maxExtent.get(dim)))
                RelativePositionConstraint.RelativePosition.DimensionOrder.SAME -> {
                    constraintRestrictedSpace.maxExtent.setComponent(dim,
                            Math.min(allowedSpace.maxExtent.get(dim), relatedBBounds.maxExtent.get(dim) + size.get(dim)))
                    constraintRestrictedSpace.minExtent.setComponent(dim,
                            Math.max(allowedSpace.minExtent.get(dim), relatedBBounds.minExtent.get(dim) - size.get(dim)))
                }
            }
        }
        return constraintRestrictedSpace
    }

    /**
     * Compute the allowed space for a single constraint given a size,
     * while contraining that the object must be in more or less direct
     * contact with the object.

     * @param size  The size of the object to place
     * *
     * @param relatedBBounds The bounding box of the related object
     * *
     * @param distance The distance between the two closest points of the objects
     * *
     * @param pos The relative position (above,below, etc...)
     * *
     * @return The space in which the object is allowed to be.
     */
    fun computeAllowedAABBForFixedDistance(size: Vector3d,
                                           relatedBBounds: AABB,
                                           distance: FixedDistance,
                                           pos: RelativePositionConstraint.RelativePosition): AABB {
        val constraintRestrictedSpace = AABB()

        val dist = distance.distance

        // Iterate over all dimensions (X,Y,Z)
        for (dim in 0..2) {
            when (pos.rel[dim]) {
                RelativePositionConstraint.RelativePosition.DimensionOrder.BEFORE // Extent has to end before the other object
                -> {
                    constraintRestrictedSpace.maxExtent.setComponent(dim,
                            relatedBBounds.minExtent.get(dim) - dist)

                    constraintRestrictedSpace.minExtent.setComponent(dim,
                            constraintRestrictedSpace.maxExtent.get(dim) - (size.get(dim) + 0.01))
                }
                RelativePositionConstraint.RelativePosition.DimensionOrder.AFTER // Extent has to begin after the other object
                -> {
                    constraintRestrictedSpace.minExtent.setComponent(dim,
                            relatedBBounds.maxExtent.get(dim) + dist)

                    constraintRestrictedSpace.maxExtent.setComponent(dim,
                            constraintRestrictedSpace.minExtent.get(dim) + (size.get(dim) + 0.01))
                }
                RelativePositionConstraint.RelativePosition.DimensionOrder.SAME // Extent has to overlap
                -> {
                    constraintRestrictedSpace.maxExtent.setComponent(dim,
                            constraintRestrictedSpace.maxExtent.get(dim) + size.get(dim))

                    constraintRestrictedSpace.minExtent.setComponent(dim,
                            constraintRestrictedSpace.minExtent.get(dim) - size.get(dim))
                }
            }
        }
        return constraintRestrictedSpace
    }
}
