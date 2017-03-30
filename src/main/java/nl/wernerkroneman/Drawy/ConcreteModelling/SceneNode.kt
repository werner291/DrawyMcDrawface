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

/**
 * A node in the scene graph.
 */
class SceneNode {

    /**
     * @inv parent != null implies parent.children.contains(this)
     * *
     * @inv forall child in children: child.parent == this
     */
    /**
     * Set the parent of this node in the tree.
     *
     *
     * Note: do not call directly, use the add/remove child
     * methods of the parent, as the parents need to be
     * updated as well to maintain a correct structure.

     * @param parent
     */
    var parent: SceneNode? = null
        protected set
    var transform = Matrix4d()
        internal set
    private val children = ArrayList<SceneNode>()
    private val drawables = ArrayList<Drawable>()

    /**
     * Get a list of drwable objects attached to this node.

     * @return A list of drawable objects
     */
    fun getDrawables(): List<Drawable> {
        return Collections.unmodifiableList(drawables)
    }

    /**
     * Get a list of children.

     * @return a list of children
     */
    fun getChildren(): List<SceneNode> {
        return Collections.unmodifiableList(children)
    }

    /**
     * Add a child node to this SceneNode.

     * @param node The node to add, must not already have a parent.
     */
    fun addChild(node: SceneNode) {
        assert(node.parent == null)
        children.add(node)
        node.parent = this
    }

    /**
     * Return the full world transform, assuming the root node's transform
     * is a world transform, which is then multiplied down the tree.
     *
     * This matrix is a new matrix and does not modify existing data.

     * @return A transformation matrix
     */
    fun computeWorldTransform(): Matrix4d {
        if (parent != null) {
            return parent!!.computeWorldTransform().mul(this.transform)
        } else {
            return Matrix4d(this.transform)
        }
    }

    fun addDrawable(drawable: Drawable) {
        assert(drawable.getAttached() == null)
        drawables.add(drawable)
        drawable._notifyAttached(this)
    }

    /**
     * Compute the AABB of the contents of this scene node
     * in this node's local coordinate frame.

     * @return the AABB.
     */
    fun computeLocalAABB(): AABB {

        // Initialize the AABB as empty and negatively space-spanning
        // Any call to extendToCover will center the box on the position
        // and have volume 0.
        val result = AABB(Vector3d(java.lang.Double.NEGATIVE_INFINITY),
                Vector3d(java.lang.Double.POSITIVE_INFINITY))

        for (child in children) {
            // Compute the bounding box of the child, and transform
            // using the child's transform to bring it to this node's
            // local coordinate space.
            val childBox = child.computeLocalAABB().transform(child.transform, AABB())

            // Extend the result AABB to cover the child space.
            result.extendToCover(childBox.minExtent)
            result.extendToCover(childBox.maxExtent)
        }

        for (drawable in drawables) {
            // Get the AABB of the child.
            val childBox = drawable.computeAABB()

            result.extendToCover(childBox.minExtent)
            result.extendToCover(childBox.maxExtent)
        }

        return result
    }

    fun computeWorldAABB(): AABB {
        return computeLocalAABB().transform(computeWorldTransform(), AABB())
    }

    /**
     * Get the AABB in the local space of the parent. That is, apply
     * this SceneNode's own transformation to lift the AAB of local
     * space to the parent context. This works if the parent is null.
     *
     * Returns new AABB that is safe to modify.
     */
    fun computeParentContextAABB(): AABB {
        return computeLocalAABB().transform(transform, AABB())
    }

    internal fun bfsIterator(): BfsIterator {
        return BfsIterator(this)
    }

    fun setTranslation(sub: Vector3d) {
        transform.setTranslation(sub)
    }
}
