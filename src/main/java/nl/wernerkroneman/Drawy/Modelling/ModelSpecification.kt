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

package nl.wernerkroneman.Drawy.Modelling

import java.util.*

/**
 * An abstract basis for an object creation specification.
 *
 * Note: represents both type AND identity. Think of a blueprint
 * featuring a pair of rectangles: from those drawings, you know
 * both that there are two distinct objects, and that those objects
 * are rectangular.
 *
 * To distinguish between identical objects, simply use [derive].
 */
abstract class ModelSpecification(val id: UUID = UUID.randomUUID(),
                                  var name: String,
                                  var location: Location? = null) {

    /**
     * Whether to allow modifications.
     */
    var locked: Boolean = false

    /**
     * Specification for the size of this object.
     */
    var size: Size = DEFAULT_SIZE

    /**
     * Create another model that specifies exactly
     * the same as this model.
     *
     * Note: by default, changes made to the original
     * are reflected in the derivative!
     */
    abstract fun derive(name: String): ModelSpecification

    /**
     * Whether the requirements of this model
     * are the same or stricter than the other
     * model without contradicting it.
     */
    abstract fun strongerThan(other: ModelSpecification): Boolean

    /**
     * Opposite of [strongerThan]
     */
    fun weakerThan(other: ModelSpecification): Boolean {
        return other.strongerThan(this)
    }

    /**
     * Whether there is a chance that objects made
     * according to this model will not comply with
     * the other model.
     */
    fun contradicts(other: ModelSpecification): Boolean {
        return !(strongerThan(other) || weakerThan(other))
    }

}