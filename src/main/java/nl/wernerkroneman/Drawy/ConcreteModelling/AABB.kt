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

import org.joml.Matrix4d
import org.joml.Vector3d
import java.util.*

data class AABB(val xMin: Double, val yMin: Double, val zMin: Double,
                val xMax: Double, val yMax: Double, val zMax: Double) {

    constructor(maxExtent: Vector3d, minExtent: Vector3d) : this(
            xMin = minExtent.x, xMax = maxExtent.x,
            yMin = minExtent.y, yMax = maxExtent.y,
            zMin = minExtent.z, zMax = maxExtent.z)

    /**
     * Get a point that can subjectively be considered the "most important point" in a range.

     * For (-inf,int), that's 0.
     * For a half-bounded line, it's the bound.
     * For a completely bounded line, that's the middle.

     * @param a The first bound
     * *
     * @param b The second bound
     * *
     * @return See description
     */
    private fun extentMean(a: Double, b: Double): Double {
        if (a.isFinite()) {
            if (b.isFinite()) {
                return (a + b) / 2.0
            } else {
                return a
            }
        } else {
            if (java.lang.Double.isFinite(b)) {
                return b
            } else {
                return 0.0
            }
        }
    }

    /**
     * Extend the box such that it at least contains the vector.

     * @param vec The vector to be covered.
     */
    fun extendToCover(vec: Vector3d): AABB = this.copy(
            xMin = Math.min(vec.x, xMin),
            yMin = Math.min(vec.y, yMin),
            zMin = Math.min(vec.z, zMin),
            xMax = Math.max(vec.x, xMax),
            yMax = Math.max(vec.y, yMax),
            zMax = Math.max(vec.z, zMax))

    /**
     * Extend the box such that it contains the other AABB
     */
    fun extendToCover(box: AABB): AABB = this.copy(
            xMin = Math.min(box.xMin, xMin),
            yMin = Math.min(box.yMin, yMin),
            zMin = Math.min(box.zMin, zMin),
            xMax = Math.max(box.xMax, xMax),
            yMax = Math.max(box.yMax, yMax),
            zMax = Math.max(box.zMax, zMax))

    /**
     * Center of the box.
     *
     * @pre AABB must be finite
     *
     * @return The center
     */
    val center = Vector3d((xMin + xMax) / 2.0,
            (yMin + yMax) / 2.0,
            (zMin + zMax) / 2.0)

    /**
     * Get a finite AABB inside this AABB that is roughly centered inside this one,
     * or sticking to an edge if half-infinite.

     * @param xSize  The lateral of the AABB (lateral)
     * *
     * @param ySize The height (vertical)
     * *
     * @param zSize The lateral of the AABB (longitudinal)
     * *
     * @param dest      Where to store the result
     * *
     * @return The result (same as dest)

    fun getFiniteWithBounds(xSize: Double, ySize: Double, zSize: Double): AABB {

        // Can an AABB of the specified size actually fit inside?
        if (this.sizeX < xSize || this.sizeY < ySize || this.sizeZ < zSize) {
            throw ArithmeticException("Requested size AABB can never fit in this AABB.")
        }

        // The AABB will be centered around this position as much as possible
        // If the AABB permits it, the result will be centered.
        val centerIsh = centerIsh()

        // Compute the minExtent while keepng it as centered as possible.
        // As a mental model, imagine the bounds of {@code this} "pushing"
        // the result around.
    return AABB(
    xMin = (centerIsh.x - xSize / 2).coerceIn(xMin, xMax),
    yMin = (centerIsh.y - xSize / 2).coerceIn(yMin, yMax),
    zMin = (centerIsh.x - xSize / 2).coerceIn(zMin, zMax)
    )
    }*/

    fun centerIsh(): Vector3d {
        val centerIsh = Vector3d(
                extentMean(xMin, xMax),
                extentMean(yMin, yMax),
                extentMean(zMin, zMax)
        )
        return centerIsh
    }

    val sizeX = xMax - xMin
    val sizeY = yMax - yMin
    val sizeZ = zMax - zMin

    /**
     * Compute the bounding box of a transformed version of this AABB
     *
     *
     * Note that this might yield a bounding box that is a lot bigger than
     * the original shape.
     *
     *
     * A more accurate (but more expensive) way would be to transform the shape itself
     * and then compute its' bounding box.

     * @param mat  The transformation
     * *
     * @param dest The AAB to store the result in.
     * *
     * @return The bounding box.
     */
    fun transform(mat: Matrix4d): AABB = this.translate(mat.getTranslation(Vector3d()))

    /**
     * Test whether any point inside this AABB is also inside the other AABB.
     * Also returns true if the boxes just touch.

     * @param b The other
     * *
     * @param tolerance How far into eachother the AABBs have to be before they "intersect"
     * *
     * @return Whether there is an intersection.
     */
    fun intersects(b: AABB, tolerance: Double): Boolean {
        return !(this.xMin + tolerance > b.xMax ||
                this.yMin + tolerance > b.yMax ||
                this.zMin + tolerance > b.zMax ||
                b.xMin + tolerance > this.xMax ||
                b.yMin + tolerance > this.yMax ||
                b.zMin + tolerance > this.zMax)
    }

    /**
     * Computes the intersection of two AABBs

     * @param other Which one to intersect this AABB with
     * *
     * @param dest  Where to store the result
     * *
     * @return Rturns dest after it has been modified
     * *
     * @modifies dest
     */
    fun intersection(other: AABB) = AABB(
            xMax = Math.min(other.xMax, this.xMax),
            yMax = Math.min(other.yMax, this.yMax),
            zMax = Math.min(other.zMax, this.zMax),
            xMin = Math.max(other.xMin, this.xMin),
            yMin = Math.max(other.yMin, this.yMin),
            zMin = Math.max(other.zMin, this.zMin))

    /**
     * Test whether the given position is insde the box

     * @param pos The position to test
     * *
     * @return Whether the position is inside (or on the boundary)
     */
    fun inside(pos: Vector3d): Boolean {
        return xMin <= pos.x && pos.x <= xMax &&
                yMin <= pos.y && pos.y <= yMax &&
                zMin <= pos.z && pos.z <= zMax
    }

    /**
     * Compute the translation of this AABB
     *
     * @param translation The translation
     * *
     * @param aabb        Where to store the result (will be overwritten)
     * *
     * @return aabb after is has been modified to represent the translation.
     */
    fun translate(translation: Vector3d) = AABB(
            xMin = xMin + translation.x,
            yMin = yMin + translation.y,
            zMin = zMin + translation.z,
            xMax = xMax + translation.x,
            yMax = yMax + translation.y,
            zMax = zMax + translation.z)

    val minExtent = Vector3d(xMin, yMin, zMin)
    val maxExtent = Vector3d(xMax, yMax, zMax)

}

val AABB_INFINITY = AABB(
        Double.NEGATIVE_INFINITY,
        Double.NEGATIVE_INFINITY,
        Double.NEGATIVE_INFINITY,
        Double.POSITIVE_INFINITY,
        Double.POSITIVE_INFINITY,
        Double.POSITIVE_INFINITY)

val AABB_REVERSE_INFINITY = AABB(
        Double.POSITIVE_INFINITY,
        Double.POSITIVE_INFINITY,
        Double.POSITIVE_INFINITY,
        Double.NEGATIVE_INFINITY,
        Double.NEGATIVE_INFINITY,
        Double.NEGATIVE_INFINITY)