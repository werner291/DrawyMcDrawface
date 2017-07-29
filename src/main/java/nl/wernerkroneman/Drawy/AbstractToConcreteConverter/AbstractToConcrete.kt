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

import io.reactivex.rxkotlin.toMaybe
import io.reactivex.rxkotlin.toSingle
import nl.wernerkroneman.Drawy.ConcreteModelling.*
import nl.wernerkroneman.Drawy.Modelling.*
import org.joml.Matrix4d
import org.joml.Vector3d
import java.util.*

typealias ColorSpec = java.awt.Color

class AbstractToConcrete(meshFactory: MeshFactory) {

    val primitiveGenerator = PrimitiveGenerator(meshFactory)

    val UNIT_CUBE = Drawable(primitiveGenerator.generateUnitCube())
    val UNIT_CYLINDER = Drawable(primitiveGenerator.generateCylinder(0.5, 1.0, 16))

    private val absToConcrete = HashMap<ModelSpecification, SceneNode>()

    fun clearCache() {
        absToConcrete.clear()
    }

    /*
     * Get the node for the model, or create it if it exists.
     */
    fun concreteForModel(model: ModelSpecification): SceneNode =
            absToConcrete.getOrPut(model) {
                SceneNode(
                        // TODO transform
                        drawables = when (model.shape) {
                            Shape.CUBE -> listOf(UNIT_CUBE)
                            Shape.SPHERE -> TODO()
                            Shape.CYLINDER -> listOf(UNIT_CYLINDER)
                            null -> listOf()
                        },
                        children = model.has.map { concreteForModel(it) }
                )
            }



    private fun relativeLocationToConcreteLocation(location: RelativeLocation) =
            relativeToAABB(location.relPos, concreteForModel(location.right).aabb)

    fun relativeToAABB(relative: RelativePosition, aabb: AABB) = when (relative) {
        RelativePosition.LEFT_OF -> AABB_INFINITY.copy(zMax = aabb.zMin)
        RelativePosition.BEHIND -> AABB_INFINITY.copy(zMax = aabb.zMin)
        RelativePosition.UNDER -> AABB_INFINITY.copy(zMax = aabb.zMin)
        RelativePosition.RIGHT_OF -> AABB_INFINITY.copy(xMin = aabb.xMax)
        RelativePosition.IN_FRONT_OF -> AABB_INFINITY.copy(yMin = aabb.yMax)
        RelativePosition.ABOVE -> AABB_INFINITY.copy(zMin = aabb.zMax)
    }

    /**
     * Convert the constraint to a restriction on the translation.
     */
    fun locationToConcreteLocation(location: Location?): AABB {

        return when (location) {
            null -> AABB_INFINITY
            is IntersectionLocation -> return location.intersectionOf
                    .map { locationToConcreteLocation(it) }
                    .reduce { a, b -> a.intersection(b) }
            is RelativeLocation ->
                return relativeLocationToConcreteLocation(location)

            else -> throw UnsupportedOperationException("Unknown location: $location")

        }
    }

}

private fun Float.remapRange(rangeAmin: Float, rangeAmax: Float,
                             rangeBmin: Float, rangeBmax: Float): Float {

    val sizeA = rangeAmax - rangeAmin
    val sizeB = rangeBmax - rangeBmin

    return rangeBmin + (this - rangeAmin) * (sizeB / sizeA)

}

class SizeInfo(val sizeModifier: Double = 1.0,
               val size: Double? = null)

fun colorSpecToColor(color: ColorSpec) =
        nl.wernerkroneman.Drawy.ConcreteModelling.Color(
                color.red.toFloat().remapRange(0f, 255f, 0f, 1f),
                color.green.toFloat().remapRange(0f, 255f, 0f, 1f),
                color.blue.toFloat().remapRange(0f, 255f, 0f, 1f),
                color.alpha.toFloat().remapRange(0f, 255f, 0f, 1f))

fun colorToMaterial(color: ColorSpec) =
        Material(ambient = colorSpecToColor(color),
                diffuse = colorSpecToColor(color),
                emissive = BLACK,
                specular = BLACK)