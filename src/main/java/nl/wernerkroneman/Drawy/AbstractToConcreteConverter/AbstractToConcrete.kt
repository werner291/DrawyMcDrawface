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

import nl.wernerkroneman.Drawy.ConcreteModelling.*
import nl.wernerkroneman.Drawy.Modelling.*
import nl.wernerkroneman.Drawy.Modelling.RelativePosition.DimensionOrder.*
import org.joml.Matrix4d
import org.joml.Vector3d
import sun.plugin.dom.exception.InvalidStateException
import java.util.*

typealias ColorSpec = java.awt.Color

class AbstractToConcrete(meshFactory: MeshFactory) {

    val primitiveGenerator = PrimitiveGenerator(meshFactory)

    private val absToConcrete = HashMap<ModelSpecification, ConcreteModel>()

    private val beingComputed = HashSet<ModelSpecification>()

    private val scene = Scene()

    class ConcreteModel(var abstract: ModelSpecification,
                        val nodes: MutableSet<SceneNode> = HashSet<SceneNode>()) {
        val aabb: AABB
            get() = nodes.map { it.computeWorldAABB() }
                    .reduce { acc, aabb -> acc.cover(aabb) }
    }

    /**
     * Compute a conrcete, drawable scene based on the abstract model.
     *
     * Note: not threadsafe
     *
     * @param absModel An abstract model
     * *
     * @return A concrete, drawable scene
     */
    fun computeScene(absModel: ModelSpecification): Scene {

        createConcreteForModel(absModel, ROOT_CONTEXT)

        return scene
    }

    fun clearCache() {
        absToConcrete.clear()
        beingComputed.clear()
    }

    val ROOT_CONTEXT = SizeInfo()

    /*
     * Get the node for the model, or create it if it exists.
     */
    fun getOrCreateConcreteForModel(model: ModelSpecification): ConcreteModel {

        if (model !in absToConcrete) {
            createConcreteForModel(model, ROOT_CONTEXT)
        }

        return absToConcrete[model]!!
    }

    /**
     * @pre {@code model !in beingComputed}
     * @post {@code model !in beingComputed}
     */
    fun createConcreteForModel(model: ModelSpecification, sizeInfo: SizeInfo): ConcreteModel {

        if (model in beingComputed) {
            throw RuntimeException("ModelSpecification contains a cycle!")
        }

        if (model in absToConcrete) {
            throw InvalidStateException("ModelSpecification being re-absToConcrete!")
        }

        // Add current to beingComputed for cycle detection
        beingComputed.add(model)

        val concrete: ConcreteModel = when (model) {
            is CompositeModelSpecification ->
                concreteForCompositeModel(model, sizeInfo)
            is GroupModelSpecification ->
                computeSceneNodeForGroupModel(model, sizeInfo)
            is PrimitiveModelSpecification ->
                computeSceneNodeForPrimitive(model, sizeInfo)
            is AnyModelSpecification ->
                createSceneNodeForAnyModel(model, sizeInfo)
            else ->
                throw UnsupportedOperationException("Unknown model type: " + model)
        }

        // Undo for performance and cleanliness.
        beingComputed.remove(model)

        absToConcrete[model] = concrete

        return concrete

    }

    fun createSceneNodeForAnyModel(model: AnyModelSpecification, sizeInfo: SizeInfo): ConcreteModel {
        val choice = model.pick()
        return getOrCreateConcreteForModel(choice)
    }

    fun computeSceneNodeForPrimitive(absModel: PrimitiveModelSpecification, sizeInfo: SizeInfo): ConcreteModel {

        val node = scene.rootSceneNode.createChildNode()

        val mat = colorToMaterial(absModel.color)

        node.addDrawable(when (absModel.shape) {
            PrimitiveModelSpecification.ShapeType.CUBE ->
                Drawable(primitiveGenerator.generateUnitCube(), mat)
            PrimitiveModelSpecification.ShapeType.SPHERE ->
                Drawable(primitiveGenerator.generateSphere((sizeInfo.size ?: 0.5) * sizeInfo.sizeModifier, 16, 8), mat)
            PrimitiveModelSpecification.ShapeType.CYLINDER ->
                Drawable(primitiveGenerator.generateCylinder((sizeInfo.size ?: 0.5) * sizeInfo.sizeModifier,
                        (sizeInfo.size ?: 1.0) * sizeInfo.sizeModifier,
                        16), mat)
            else -> throw UnsupportedOperationException("Shape " + absModel.shape + " not implemented.")
        })

        node.transform = computeObjectTransform(node, absModel)

        return ConcreteModel(absModel, mutableSetOf(node))
    }

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

    /**
     * In case of a CompositeModelSpecification, every model instance
     * in the model will be given a SceneNode.
     *
     *
     * In positioning the directComponents, only those in the CompositeModelSpecification are taken into account.
     * Taking this higher up in the tree into account is planned later on,
     * but this should work for now.
     */
    private fun computeSceneNodeForGroupModel(absModel: GroupModelSpecification,
                                              sizeInfo: SizeInfo): ConcreteModel {

        // TODO implement identical models.
        return ConcreteModel(absModel, 0.until(absModel.number)
                .map { absModel.memberModelType.derive("${absModel.memberModelType.name} $it}") }
                .map { getOrCreateConcreteForModel(it) }
                .map { it.nodes }
                .flatten()
                .toMutableSet())
    }

    /**
     * In case of a CompositeModelSpecification, every model instance
     * in the model will be given a SceneNode.
     *
     *
     * In positioning the directComponents, only those in the CompositeModelSpecification are taken into account.
     * Taking this higher up in the tree into account is planned later on,
     * but this should work for now.
     */
    private fun concreteForCompositeModel(absModel: CompositeModelSpecification,
                                          sizeInfo: SizeInfo): ConcreteModel {
        
        val childContext = SizeInfo()

        return ConcreteModel(absModel, absModel.directComponents
                .map { getOrCreateConcreteForModel(it) }
                .map { it.nodes }
                .flatten()
                .toMutableSet())
    }

    /**
     * Compute the translation of the child SceneNode corresponding to a given component.
     *
     * @param childNode The node of which to compute the transform
     * @param absModel The model that the node corresponds to.
     */
    private fun computeObjectTransform(
            childNode: SceneNode,
            absModel: ModelSpecification): Matrix4d {

        // Get an AABB to estimate how big the component is and how the AABB fits around it.
        val componentAABB = childNode.computeParentContextAABB()
                .translate(childNode.transform.getTranslation(Vector3d()))

        // Compute an AABB to restrict the AABB, start with one spanning space
        val translationRestriction = locationToConcreteLocation(componentAABB,
                absModel.location)

        return Matrix4d().identity()
                .setTranslation(translationRestriction.centerIsh())
    }

    private fun relativeLocationToConcreteLocation(location: RelativeLocation,
                                                   selfAABB: AABB): AABB {

        val otherAABB = getOrCreateConcreteForModel(location.right).aabb

        val restrictTo = AABB(Vector3d(Double.POSITIVE_INFINITY),
                Vector3d(Double.NEGATIVE_INFINITY))

        location.relPos.rel.forEachIndexed { index, dimensionOrder ->
            when (dimensionOrder) {
            // S: |-------x-----|
            // LastCreatedComponentInterpreter:                  |-----x------|
                AFTER -> restrictTo.minExtent.setComponent(index,
                        otherAABB.maxExtent[index] - selfAABB.minExtent[index])
            // S: |-------x-----|
            // LastCreatedComponentInterpreter:        |-----x------|
            // or
            // S:    |-------x-----|
            // LastCreatedComponentInterpreter: |-----x------|
                SAME -> {
                    restrictTo.minExtent.setComponent(index,
                            otherAABB.minExtent[index] - selfAABB.maxExtent[index])

                    restrictTo.maxExtent.setComponent(index,
                            otherAABB.maxExtent[index] - selfAABB.minExtent[index])
                }
                BEFORE -> restrictTo.maxExtent.setComponent(index,
                        otherAABB.minExtent[index] - selfAABB.maxExtent[index])
            }

        }
        return restrictTo
    }

    /**
     * Convert the constraint to a restriction on the translation.
     */
    fun locationToConcreteLocation(selfAABB: AABB,
                                   location: Location?): AABB {

        return when (location) {
            null -> AABB(Vector3d(Double.POSITIVE_INFINITY),
                    Vector3d(Double.NEGATIVE_INFINITY))
            is IntersectionLocation -> return location.intersectionOf
                    .map { locationToConcreteLocation(selfAABB, it) }
                    .reduce { a, b -> a.intersection(b, AABB()) }
            is RelativeLocation ->
                return relativeLocationToConcreteLocation(location, selfAABB)

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