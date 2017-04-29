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
 * A composite model is a specification for an object
 * based on a combination of multiple distinct specifications.
 *
 * Similar to [GroupModelSpecification], except that components are
 * specified explicitly one-by-one and are usually
 * identifiable.
 */
abstract class CompositeModelSpecification(id: UUID = UUID.randomUUID(),
                                           name: String = "Anonymous composite") : ModelSpecification(id, name) {

    /**
     * A set of models that are considered
     * a direct component of the composite.
     *
     * Note that this set does not contain
     * transitive directComponents, for that see
     * [transitiveComponentsDfsIterator]
     */
    abstract val directComponents: MutableSet<ModelSpecification>

    /**
     * Represents all parts of the model, both directly and transitively.
     *
     * Think of a car: is the front axle part of the car or of the front
     * wheel assembly? It is both, which is why both the front axle and
     * the front wheel assembly would be found in the [components] of
     * a CompositeModelSpecification representing a car.
     */
    val components: Iterable<ModelSpecification>
        get() = object : Iterable<ModelSpecification> {
            override fun iterator(): Iterator<ModelSpecification> =
                    transitiveComponentsBfsIterator()
        }

    /**
     * Get an iterator that iterates over all
     * transitive components in BFS order.
     *
     * Note that no guarantees are made about uniqueness,
     * the same transitive component may be visited more
     * than once if it is referred to by several other
     * components.
     */
    fun transitiveComponentsBfsIterator() =
            object : Iterator<ModelSpecification> {

                val queue: Queue<ModelSpecification> = LinkedList<ModelSpecification>().apply {
                    add(this@CompositeModelSpecification)
                }

                override fun hasNext() = !queue.isEmpty()

                override fun next(): ModelSpecification {
                    val next = queue.poll()

                    if (next is CompositeModelSpecification) {
                        queue.addAll(next.directComponents)
                    }

                    return next
                }
            }

    override fun strongerThan(other: ModelSpecification): Boolean {

        if (other !is CompositeModelSpecification) return false

        TODO("Check whether this is possibly NP-complete...")

    }
}

/**
 * Composite model specified explicitly
 * as a combination of certain components.
 */
class CompositeModelSpecificationBase(id: UUID = UUID.randomUUID(),
                                      name: String = "Anonymous composite",
                                      override val directComponents: MutableSet<ModelSpecification> = HashSet<ModelSpecification>()) : CompositeModelSpecification(id, name) {
    override fun derive(name: String): ModelSpecification {
        return DerivedCompositeModelSpecification(UUID.randomUUID(), name, this)
    }
}

/**
 * Composite model specified as having the same components
 * and other properties as another CompositeModelSpecification, possibly
 * with some changes.
 */
class DerivedCompositeModelSpecification(id: UUID = UUID.randomUUID(),
                                         name: String = "Anonymous composite derivative",
                                         val base: CompositeModelSpecification) : CompositeModelSpecification(id, name) {

    override fun derive(name: String): ModelSpecification {
        return DerivedCompositeModelSpecification(UUID.randomUUID(), name, this)
    }

    override val directComponents = MutableRelativeSet(base.directComponents)
}