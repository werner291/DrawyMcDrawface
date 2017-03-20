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
import org.joml.Vector3d

class AbstractToConcrete(internal var meshFactory: MeshFactory) {
    val primitiveGenerator = PrimitiveGenerator(meshFactory)

    /**
     * Compute a conrete, drawable scene based on the abstract model.

     * @param absModel An abstract model
     * *
     * @return A concrete, drawable scene
     */
    fun computeScene(absModel: Model): Scene {

        val result = Scene()

        result.rootSceneNode = createSceneNodeForModel(absModel, Context())

        return result
    }

    /**
     * Compute the contents of the scene node in accordance to the model.

     * @param absModel The model to base the scene node on.
     * *
     * @param node     The scene node that represents the model.
     */
    private fun createSceneNodeForModel(absModel: Model, context: Context): SceneNode {
        return when (absModel) {
            is CompositeModel -> exploreCompositeModel(absModel, context)
            is GroupModel -> computeSceneNodeForGroupModel(absModel, context)
            is PrimitiveModel -> computeSceneNodeForPrimitive(absModel, context)
            is AnyModel -> createSceneNodeForModel(absModel.any, context)
            is VariantModel -> createSceneNodeForVariantModel(absModel, context)
            else -> throw UnsupportedOperationException("Unknown model type: " + absModel)
        }
    }

    private fun createSceneNodeForVariantModel(absModel: VariantModel, context: Context): SceneNode {

        val childContext: Context

        for (mod in absModel.modifiers) {
            when (mod) {
                is RelativeSize -> context.sizeModifier *= mod.relativeSize
                else -> throw UnsupportedOperationException("Unknown modifier " + mod)
            }
        }

        return createSceneNodeForModel(absModel.base, context)
    }

    private fun computeSceneNodeForPrimitive(absModel: PrimitiveModel, context: Context): SceneNode {

        println(context.sizeModifier)

        val node = SceneNode()

        node.addDrawable(when (absModel.shape) {
            PrimitiveModel.ShapeType.CUBE ->
                Drawable(primitiveGenerator.generateUnitCube())
            PrimitiveModel.ShapeType.SPHERE ->
                Drawable(primitiveGenerator.generateSphere((context.size ?: 1.0) * context.sizeModifier, 16, 8))
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
                                              context: Context): SceneNode {

        val node = SceneNode()

        val childContext = Context(node)

        // TODO implement identical models.
        0.until(absModel.number)
                .forEach {
                    node.addChild(createSceneNodeForModel(absModel.memberModelType,
                            childContext))
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
                                      context: Context): SceneNode {

        val node = SceneNode()

        val childContext = Context(node)

        absModel.componentsInTopoligicalOrder()
                .forEach { node.addChild(createSceneNodeForModel(it.model!!, childContext)) }

        return node

    }

    fun getTranslationToFit(child: SceneNode, place: AABB): Vector3d {
        return place.minExtent.sub(child.computeLocalAABB().minExtent, Vector3d())
    }
}

private class Context(val node: SceneNode? = null,
                      var sizeModifier: Double = 1.0,
                      var size: Double? = null)