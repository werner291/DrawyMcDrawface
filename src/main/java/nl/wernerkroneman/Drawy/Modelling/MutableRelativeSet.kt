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
 * Mutable (view of a) set specified relative to and
 * reflecting changes in [base], with some explicitly-defined
 * additions and removals.
 *
 * If there is an element in [base], it is present in this,
 * unless it was explicitly removed from this.
 */
class MutableRelativeSet<T>(val base: Set<T>) : MutableSet<T> {

    val additions = HashSet<T>()
    val removals = HashSet<T>()

    override fun add(element: T): Boolean {
        var added = false
        if (element !in base) {
            added = true
            additions.add(element)
        }
        if (element in removals) {
            added = true
            removals.remove(element)
        }
        return added
    }

    override fun addAll(elements: Collection<T>): Boolean {
        var added = false

        // Not done with any() to avoid unhelpful "optimisations"
        elements.forEach { elem -> added = added || add(elem) }

        return added
    }

    override fun clear() {
        throw UnsupportedOperationException("You probably did not mean to clear a relative set. Try clearing the base set instead.")
    }

    override fun iterator(): MutableIterator<T> {

        val baseItr = base.minus(removals).plus(additions).iterator()

        return object : MutableIterator<T> {
            override fun hasNext(): Boolean {
                return baseItr.hasNext()
            }

            override fun next(): T {
                return baseItr.next()
            }

            override fun remove() {
                TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
            }
        }
    }

    override fun remove(element: T): Boolean {
        var removed = false
        if (element in base) {
            removed = true
            removals.add(element)
        }
        if (element in additions) {
            removed = true
            additions.remove(element)
        }
        return removed
    }

    override fun removeAll(elements: Collection<T>): Boolean {
        var removed = false

        // Not done with any() to avoid unhelpful "optimisations"
        elements.forEach { elem -> removed = removed || remove(elem) }

        return removed
    }

    override fun retainAll(elements: Collection<T>): Boolean {
        return TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.
    }

    override val size: Int
        get() = base.minus(removals).plus(additions).size

    override fun contains(element: T): Boolean {
        return (element in base || element in additions) && element !in removals
    }

    override fun containsAll(elements: Collection<T>): Boolean {
        return elements.all { it in this }
    }

    override fun isEmpty(): Boolean {
        return base.minus(removals).plus(additions).isEmpty()
    }
}