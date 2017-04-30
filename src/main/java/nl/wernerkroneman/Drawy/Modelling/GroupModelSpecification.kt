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
 * Represents a group (usually understood as a cluster)
 * of a certain number of copies of a certain model.

 * This model is very similar to a [CompositeModelSpecification],
 * except that the contents are understood to all be
 * compliant with some same specification, though
 * note that specifications may not be deterministic.
 *
 * Also, note that individual components are normally
 * not explicitly identifiable since their number is
 * often not determined in advance.
 */
abstract class GroupModelSpecification(id: UUID = UUID.randomUUID(),
                                       name: String) : ModelSpecification(id, name) {

    abstract var number: Int
    abstract var memberModelType: nl.wernerkroneman.Drawy.Modelling.ModelSpecification

    override fun derive(name: String): ModelSpecification {
        return GroupModelSpecificationDerived(UUID.randomUUID(), name, this)
    }

    //val constraints: MutableSet<RelativePositionConstraint> = mutableSetOf()

    /*override fun getApplicableConstraintsFor(component: nl.wernerkroneman.Drawy.Modelling.RelativeConstraintContext.Positionable):
            Iterable<nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint> {

        if (component !is nl.wernerkroneman.Drawy.Modelling.GroupModelSpecification.ComponentDesignator.IndexComponent) {
            throw IllegalArgumentException("Non-IndexComponent makes no sense out of context.")
        }

        return constraints.filter {
            val a = it.a
            when (a) {
                is nl.wernerkroneman.Drawy.Modelling.GroupModelSpecification.ComponentDesignator.IndexComponent -> a.index == component.index
                is nl.wernerkroneman.Drawy.Modelling.GroupModelSpecification.ComponentDesignator.IndexRangeComponent -> component in a
                else -> throw IllegalStateException("Constraint with invalid RelativeComponent as 'a'.")
            }
        }
    }

    override fun toString(): String {
        return "GroupModelSpecification{" +
                "number=" + number +
                ", memberType=" + memberModelType +
                ", constraints=" + constraints +
                '}'
    }*/

    sealed class ComponentDesignator : nl.wernerkroneman.Drawy.Modelling.RelativeConstraintContext.Positionable {
        // Object that designates one of the members of the group by index.
        // Note that this is rather abstract: there is no list of members,
        // this must be determined when interpreting the model.
        class IndexComponent(val index: Int) : nl.wernerkroneman.Drawy.Modelling.GroupModelSpecification.ComponentDesignator() {
            override fun equals(other: Any?): Boolean {
                if (this === other) return true
                if (other?.javaClass != javaClass) return false

                other as nl.wernerkroneman.Drawy.Modelling.GroupModelSpecification.ComponentDesignator.IndexComponent

                if (index != other.index) return false

                return true
            }

            override fun hashCode(): Int {
                return index
            }
        }

        class IndexRangeComponent(val indexAtLeast: Int?,
                                  val indexAtMost: Int?) : nl.wernerkroneman.Drawy.Modelling.GroupModelSpecification.ComponentDesignator() {
            operator fun contains(component: nl.wernerkroneman.Drawy.Modelling.GroupModelSpecification.ComponentDesignator.IndexComponent): Boolean {
                return (indexAtLeast == null || component.index >= indexAtLeast) &&
                        (indexAtMost == null || component.index <= indexAtMost)
            }
        }

        // Indicate a member of the group by relative index.
        // Note that "relative to what" must be understood from context.
        class RelativeComponent(val offset: Int) : nl.wernerkroneman.Drawy.Modelling.GroupModelSpecification.ComponentDesignator()
    }

    fun totalSpecificationForMemberWithIndex(index: Int): ModelSpecification {
        return TODO("Not implemented")
    }
}

class GroupModelSpecificationBase(id: UUID = UUID.randomUUID(),
                                  name: String,
                                  override var number: Int,
                                  override var memberModelType: ModelSpecification) : GroupModelSpecification(id, name) {

    override fun strongerThan(other: ModelSpecification): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}

class GroupModelSpecificationDerived(id: UUID = UUID.randomUUID(),
                                     name: String,
                                     val base: GroupModelSpecification) : GroupModelSpecification(id, name) {
    override fun strongerThan(other: ModelSpecification): Boolean {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override var number: Int
            by DelegatedUntilSet { base.number }

    override var memberModelType: ModelSpecification
            by DelegatedUntilSet { base.memberModelType }

}