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
 * ModelSpecification specifying an object that complies with any
 * of the specifications provided.
 */
abstract class AnyModelSpecification(id: UUID = UUID.randomUUID(),
                                     name: String) : ModelSpecification(id, name) {

    override fun strongerThan(other: ModelSpecification): Boolean {
        return options.isNotEmpty() && options.all { it.strongerThan(other) }
    }

    abstract val options: MutableSet<ModelSpecification>

    fun pick(): ModelSpecification {
        return options.toList()[random.nextInt(options.size)]
    }

    companion object {
        internal var random = java.util.Random()
    }
}

class AnyModelSpecificationBasis(id: UUID = UUID.randomUUID(),
                                 name: String,
                                 override val options: MutableSet<ModelSpecification>) : AnyModelSpecification(id, name) {

    override fun derive(name: String): ModelSpecification {
        return AnyModelSpecificationDerivative(UUID.randomUUID(), name, this)
    }


}

class AnyModelSpecificationDerivative(id: UUID = UUID.randomUUID(),
                                      name: String,
                                      var base: AnyModelSpecification) : AnyModelSpecification(id, name) {

    override val options = MutableRelativeSet<ModelSpecification>(base.options)

    override fun derive(name: String): ModelSpecification {
        return AnyModelSpecificationDerivative(UUID.randomUUID(), name, this)
    }

}

