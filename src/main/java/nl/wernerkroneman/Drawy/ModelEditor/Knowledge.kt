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

package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Modelling.Model
import nl.wernerkroneman.Drawy.Modelling.PrimitiveModel
import nl.wernerkroneman.Drawy.Modelling.PrimitiveModelBase
import java.util.*

class Knowledge {

    // @inv For each entry: model name matches key
    var knownObjects: MutableMap<String, Model> = TreeMap(String.CASE_INSENSITIVE_ORDER)

    fun getObject(name: String): Model? {
        return knownObjects[name]
    }

    fun remember(model: Model) {
        knownObjects.put(model.name, model)
    }

    val numberOfObjects: Int
        get() = knownObjects.size

    fun isKnownObject(model: Model): Boolean {
        return knownObjects.values.any({ it == model })
    }

    companion object {

        fun knowledgeWithPrimitives(): Knowledge {
            val knowledge = Knowledge()

            knowledge.remember(
                    PrimitiveModelBase("Cube", PrimitiveModel.ShapeType.CUBE))
            knowledge.remember(
                    PrimitiveModelBase("Cylinder", PrimitiveModel.ShapeType.CYLINDER))
            knowledge.remember(
                    PrimitiveModelBase("Sphere", PrimitiveModel.ShapeType.SPHERE))

            return knowledge
        }
    }

}