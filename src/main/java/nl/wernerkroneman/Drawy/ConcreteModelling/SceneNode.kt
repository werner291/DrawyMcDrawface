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

/**
 * A node in the scene graph.
 */
data class SceneNode(val drawables: List<Drawable>,
                     val children: List<Pair<SceneNode, Matrix4d>>) {

    val aabb: AABB = children.fold(AABB_REVERSE_INFINITY)
    { acc: AABB, childTransf: Pair<SceneNode, Matrix4d> ->
        acc.extendToCover(childTransf.first.aabb.transform(childTransf.second))
    }
}
