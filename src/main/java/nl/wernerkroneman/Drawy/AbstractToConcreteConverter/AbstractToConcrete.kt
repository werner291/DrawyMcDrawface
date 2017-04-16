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
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.RelativePosition.DimensionOrder.*
import org.joml.Matrix4d
import org.joml.Vector3d
import java.util.*


class AbstractToConcrete(meshFactory: MeshFactory) {

    val primitiveGenerator = PrimitiveGenerator(meshFactory)

    private val topLevelModelToNode = HashMap<Model, SceneNode>()

    private val beingComputed = HashSet<Model>()

    class ConcreteModel(var abstract: Model,
                        val nodes: MutableSet<SceneNode> = HashSet<SceneNode>())

    /**
     * Compute a conrcete, drawable scene based on the abstract model.
     *
     * Note: not threadsafe
     *
     * @param absModel An abstract model
     * *
     * @return A concrete, drawable scene
     */
    fun computeScene(absModel: Model): Scene {

        val result = Scene()

        clearCache()

        createNodeForModel(absModel, ROOT_CONTEXT)

        topLevelModelToNode.values.forEach {
            result.rootSceneNode.addChild(it)
        }

        return result
    }

    fun clearCache() {
        topLevelModelToNode.clear()
        beingComputed.clear()
    }

    val ROOT_CONTEXT = SizeInfo()

    /*
     * Get the node for the model, or create it if it exists.
     */
    fun getOrCreateTopLevelNodeForModel(model: Model): SceneNode {
        if (model !in topLevelModelToNode) {
            createNodeForModel(model, ROOT_CONTEXT)
        }

        return topLevelModelToNode[model]!!
    }

    /**
     * @pre {@code model !in beingComputed}
     * @post {@code model !in beingComputed}
     */
    fun createNodeForModel(model: Model, sizeInfo: SizeInfo): SceneNode {

        if (model in beingComputed) {
            throw RuntimeException("Model contains a cycle!")
        }

        if (model in topLevelModelToNode) {
            throw InvalidStateException("Model being re-topLevelModelToNode!")
        }

        // Add current to beingComputed for cycle detection
        beingComputed.add(model)

        val node: SceneNode = when (model) {
            is CompositeModel ->
                exploreCompositeModel(model, sizeInfo)
            is GroupModel ->
                computeSceneNodeForGroupModel(model, sizeInfo)
            is PrimitiveModel ->
                computeSceneNodeForPrimitive(model, sizeInfo)
            is AnyModel ->
                createSceneNodeForAnyModel(model, sizeInfo)
            else ->
                throw UnsupportedOperationException("Unknown model type: " + model)
        }

        // Undo for performance and cleanliness.
        beingComputed.remove(model)

        node.transform = computeObjectTransform(node, model)

        return node

    }

    fun createSceneNodeForAnyModel(model: AnyModel, sizeInfo: SizeInfo): SceneNode {
        val choice = model.pick()
        return createNodeForModel(choice, sizeInfo)
    }

    fun computeSceneNodeForPrimitive(absModel: PrimitiveModel, sizeInfo: SizeInfo): SceneNode {

        val node = SceneNode()

        node.addDrawable(when (absModel.shape) {
            PrimitiveModel.ShapeType.CUBE ->
                Drawable(primitiveGenerator.generateUnitCube())
            PrimitiveModel.ShapeType.SPHERE ->
                Drawable(primitiveGenerator.generateSphere((sizeInfo.size ?: 0.5) * sizeInfo.sizeModifier, 16, 8))
            PrimitiveModel.ShapeType.CYLINDER ->
                Drawable(primitiveGenerator.generateCylinder((sizeInfo.size ?: 0.5) * sizeInfo.sizeModifier,
                        (sizeInfo.size ?: 1.0) * sizeInfo.sizeModifier,
                        16))
            else -> throw UnsupportedOperationException("Shape " + absModel.shape + " not implemented.")
        })

        return node
    }

    /**
     * In case of a CompositeModel, every model instance
     * in the model will be given a SceneNode.
     *
     *
     * In positioning the components, only those in the CompositeModel are taken into account.
     * Taking this higher up in the tree into account is planned later on,
     * but this should work for now.
     */
    private fun computeSceneNodeForGroupModel(absModel: GroupModel,
                                              sizeInfo: SizeInfo): SceneNode {

        val node = SceneNode()

        val childContext = SizeInfo()

        // TODO implement identical models.
        0.until(absModel.number)
                .forEach { index ->
                    createNodeForModel(absModel.memberModelType, childContext)
                }

        return node
    }

    /**
     * In case of a CompositeModel, every model instance
     * in the model will be given a SceneNode.
     *
     *
     * In positioning the components, only those in the CompositeModel are taken into account.
     * Taking this higher up in the tree into account is planned later on,
     * but this should work for now.
     */
    private fun exploreCompositeModel(absModel: CompositeModel,
                                      sizeInfo: SizeInfo): SceneNode {

        val node = SceneNode()

        val childContext = SizeInfo()

        for (component in absModel.components) {
            createNodeForModel(component, childContext)
        }

        return node
    }

    /**
     * Compute the translation of the child SceneNode corresponding to a given component.
     *
     * @param childNode The node of which to compute the transform
     * @param component The component corresponding to the node
     * @param componentToNode A function that, for every component,
     *                  provides the corresponding SceneNode that was previously topLevelModelToNode.
     * @param composite  The CompositeModel that is the context
     */
    private fun computeObjectTransform(
            childNode: SceneNode,
            absModel: Model): Matrix4d {

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
        val otherAABB = getOrCreateTopLevelNodeForModel(location.right).computeParentContextAABB()

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

class SizeInfo(val sizeModifier: Double = 1.0,
               val size: Double? = null)