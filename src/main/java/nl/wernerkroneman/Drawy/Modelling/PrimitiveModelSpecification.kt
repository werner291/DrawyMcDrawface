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

import java.awt.Color
import java.util.*

/**
 * A specification designating an indivisible shape.
 */
abstract class PrimitiveModelSpecification(id: UUID = UUID.randomUUID(),
                                           name: String) : ModelSpecification(id, name) {

    abstract var shape: ShapeType
    abstract var color: Color

    enum class ShapeType {
        CUBE, SPHERE, CYLINDER, CONE
    }

    override fun derive(name: String): PrimitiveModelSpecification {
        return PrimitiveDerivative(UUID.randomUUID(), name, this)
    }
}

class PrimitiveModelSpecificationBase(id: UUID = UUID.randomUUID(),
                                      name: String,
                                      override var shape: PrimitiveModelSpecification.ShapeType,
                                      override var color: Color = Color.LIGHT_GRAY)
    : PrimitiveModelSpecification(id, name)

class PrimitiveDerivative(id: UUID = UUID.randomUUID(),
                          name: String,
                          var base: PrimitiveModelSpecification) : PrimitiveModelSpecification(id, name) {

    override var color: Color by DelegatedUntilSet { base.color }

    override var shape: ShapeType by DelegatedUntilSet { base.shape }
}