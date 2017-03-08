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

class PrimitiveModel(shape: PrimitiveModel.ShapeType, name: String) : Model(name) {

    // A bit unelegant right now, will eventually replace with something mesh-based
    // where something class-based actually makes sense.

    var shape: ShapeType
        internal set

    init {
        this.shape = shape
    }

    override fun toString(): String {
        return "Primitive " + shape
    }

    enum class ShapeType {
        CUBE, SPHERE, CYLINDER
    }

    override fun <V : Any> accept(visitor: ModelVisitor<V>): V {
        return visitor.visit(this)
    }


}
