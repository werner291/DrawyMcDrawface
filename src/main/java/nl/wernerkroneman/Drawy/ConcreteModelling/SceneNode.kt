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
data class SceneNode(val transform: Matrix4d = Matrix4d().identity(),
                     val drawables: List<Drawable> = emptyList(),
                     val children: List<SceneNode> = emptyList()) {

    val aabb: AABB by lazy {
        children.map { it.aabb }
                .plus(drawables.map { it.mesh.computeAABB() })
                .fold(AABB_REVERSE_INFINITY) { acc: AABB, next: AABB -> acc.extendToCover(next) }
                .transform(transform)
    }
}
