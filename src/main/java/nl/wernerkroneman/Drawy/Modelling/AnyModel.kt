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

abstract class AnyModel(id: UUID = UUID.randomUUID(),
                        name: String) : Model(id, name) {

    abstract val options: MutableSet<Model>

    fun pick(): Model {
        return options.toList()[random.nextInt(options.size)]
    }

    companion object {
        internal var random = java.util.Random()
    }
}

class AnyModelBasis(id: UUID = UUID.randomUUID(),
                    name: String,
                    override val options: MutableSet<Model>) : AnyModel(id, name) {

    override fun derive(name: String): Model {
        return AnyModelDerivative(UUID.randomUUID(), name, this)
    }


}

class AnyModelDerivative(id: UUID = UUID.randomUUID(),
                         name: String,
                         var base: AnyModel) : AnyModel(id, name) {

    override val options = MutableRelativeSet<Model>(base.options)

    override fun derive(name: String): Model {
        return AnyModelDerivative(UUID.randomUUID(), name, this)
    }

}

