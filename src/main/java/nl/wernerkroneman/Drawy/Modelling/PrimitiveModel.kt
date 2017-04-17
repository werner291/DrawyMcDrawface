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

abstract class PrimitiveModel(id: UUID = UUID.randomUUID(),
                              name: String) : Model(id, name) {

    abstract var shape: ShapeType

    enum class ShapeType {
        CUBE, SPHERE, CYLINDER, CONE
    }

    override fun derive(name: String): PrimitiveModel {
        return PrimitiveDerivative(UUID.randomUUID(), name, this)
    }
}

class PrimitiveModelBase(id: UUID = UUID.randomUUID(),
                         name: String,
                         override var shape: PrimitiveModel.ShapeType)
    : PrimitiveModel(id, name)

class PrimitiveDerivative(id: UUID = UUID.randomUUID(),
                          name: String,
                          var base: PrimitiveModel) : PrimitiveModel(id, name) {

    var _ownShape: ShapeType? = null

    override var shape: ShapeType
        get() = _ownShape ?: base.shape
        set(value) {
            _ownShape = value
        }
}