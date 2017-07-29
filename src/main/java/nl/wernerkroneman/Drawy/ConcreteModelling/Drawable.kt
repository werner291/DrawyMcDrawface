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

/**
 * Represents an instance of a Mesh in the scene.
 * Can be attached to a SceneNode.
 */
class Drawable(val mesh: Mesh,
               val material: Material = DEFAULT_GRAY) {

    var attached: SceneNode? = null
        private set

    /**
     * Change the reference that the Drawable has to its' parent.
     * Must not already have a parent, set to null to change parent.

     * @param attached The SceneNode to which this one is attached.
     */
    internal fun _notifyAttached(attached: SceneNode?) {

        assert(attached != null)

        this.attached = attached
    }
}
