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

/**
 * The CompositeModel represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 *
 *
 * It describes the scene on a very high level, and deals
 * in constraints rather than realisations of those constraints.
 */
class CompositeModel(name: String = "Anonymous composite",
                     val components: MutableSet<Component> = mutableSetOf(),
                     override var constraints: MutableSet<Constraint> = mutableSetOf()) :
        Model(name), RelativeConstraintContext {

    override fun <V : Any> accept(visitor: ModelVisitor<V>): V {
        return visitor.visit(this)
    }

    fun addComponentForModel(cube: Model): Component {
        val comp = Component(cube)
        components.add(comp)
        return comp
    }

    override fun toString(): String {
        val builder = StringBuilder()

        for (comp in components) {
            builder.append(comp.toString())
        }

        return builder.toString()
    }

    fun addConstraint(relativePositionConstraint: RelativePositionConstraint) {
        constraints.add(relativePositionConstraint)
    }

    class Component(val model: Model?) : RelativeConstraintContext.Positionable
}