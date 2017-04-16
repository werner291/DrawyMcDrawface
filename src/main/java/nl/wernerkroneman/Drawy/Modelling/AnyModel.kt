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

abstract class AnyModel(name: String) : Model(name) {

    abstract val options: MutableSet<Model>

    fun pick(): Model {
        return options.toList()[random.nextInt(options.size)]
    }

    companion object {
        internal var random = java.util.Random()
    }
}

class AnyModelBasis(name: String,
                    override val options: MutableSet<Model>) : AnyModel(name) {

    override fun derive(name: String): Model {
        return AnyModelDerivative(name, this)
    }


}

class AnyModelDerivative(name: String,
                         var base: AnyModel) : AnyModel(name) {

    override val options = MutableRelativeSet<Model>(base.options)

    override fun derive(name: String): Model {
        return AnyModelDerivative(name, this)
    }

}

