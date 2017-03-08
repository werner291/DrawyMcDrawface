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

package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Modelling.Model

interface DescriptionSessionListener {

    /**
     * Called when the target in the description session has been changed.
     *
     *
     * Throughout the session, this may be the same object, but don't assume it.

     * @param scene
     */
    fun modelChanged(scene: Model)

    /**
     * Called when the sessionis ended and the target will no longer change.
     */
    fun sessionEnded()

    /**
     * Called when a session is started within a session.

     * @param recursiveSession
     */
    fun recursiveSessionStarted(recursiveSession: DescriptionSession)
}
