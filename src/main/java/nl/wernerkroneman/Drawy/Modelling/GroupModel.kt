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

    val constraints: MutableSet<RelativePositionConstraint> = mutableSetOf()

    override fun getApplicableConstraintsFor(component: RelativeConstraintContext.Positionable):
            Iterable<RelativePositionConstraint> {

        if (component !is ComponentDesignator.IndexComponent) {
            throw IllegalArgumentException("Non-IndexComponent makes no sense out of context.")
        }

        return constraints.filter {
            val a = it.a
            when (a) {
                is ComponentDesignator.IndexComponent -> a.index == component.index
                is ComponentDesignator.IndexRangeComponent -> component in a
                else -> throw IllegalStateException("Constraint with invalid RelativeComponent as 'a'.")
            }
        }
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

    sealed class ComponentDesignator : RelativeConstraintContext.Positionable {
        // Object that designates one of the members of the group by index.
        // Note that this is rather abstract: there is no list of members,
        // this must be determined when interpreting the model.
        class IndexComponent(val index: Int) : ComponentDesignator() {
            override fun equals(other: Any?): Boolean {
                if (this === other) return true
                if (other?.javaClass != javaClass) return false

                other as IndexComponent

                if (index != other.index) return false

                return true
            }

            override fun hashCode(): Int {
                return index
            }
        }

        class IndexRangeComponent(val indexAtLeast: Int?,
                                  val indexAtMost: Int?) : ComponentDesignator() {
            operator fun contains(component: IndexComponent): Boolean {
                return (indexAtLeast == null || component.index >= indexAtLeast) &&
                        (indexAtMost == null || component.index <= indexAtMost)
            }
        }

        // Indicate a member of the group by relative index.
        // Note that "relative to what" must be understood from context.
        class RelativeComponent(val offset: Int) : ComponentDesignator()
    }
}
