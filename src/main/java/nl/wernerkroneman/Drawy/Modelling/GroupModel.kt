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
 * Represents a group (usually understood as a cluster)
 * of a certain number of copies of a certain model.

 * This model is very similar to a [CompositeModel],
 * except that the contents are understood to be identical
 * (but they may be interpreted differently individually if
 * non-determinisitc), and the number of components may also
 * vary.

 * It is also a [RelativeConstraintContext], in the sense
 * that you can specify a relation between different elements,
 * usually between one element and the next.
 */
class GroupModel(var number: Int,
                 var memberModelType: Model,
                 name: String) : Model(name), RelativeConstraintContext {
    override var constraints: MutableSet<Constraint>
        get() = throw UnsupportedOperationException()
        set(value) {
        }

    override fun toString(): String {
        return "GroupModel{" +
                "number=" + number +
                ", memberType=" + memberModelType +
                ", constraints=" + constraints +
                '}'
    }

    override fun <V : Any> accept(visitor: ModelVisitor<V>): V {
        return visitor.visit(this)
    }

    companion object {
        // Placeholder values for relative constraints
        val PLACEHOLDER_A: RelativeConstraintContext.Positionable = CompositeModel.Component(null)
        val PLACEHOLDER_B: RelativeConstraintContext.Positionable = CompositeModel.Component(null)
    }
}
