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

import nl.wernerkroneman.Drawy.Modelling.*
import org.json.simple.JSONArray
import org.json.simple.JSONObject

/**
 * Class that converts a Knowledge into a JSON object.

 * A Knowledge is provided on construction, and serializeKnowledge()
 * is called to obtain a JSONObject that represents the Knowledge.
 */
class KnowledgeJSONSerializer(internal var knowledge: Knowledge) : ModelVisitor<JSONObject> {
    override fun visit(model: VariantModel): JSONObject {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    fun serializeKnowledge(): JSONObject {

        val obj = JSONObject()

        obj.put("type", "Knowledge")

        val objects = JSONArray()

        for ((key, value) in knowledge.knownObjects) {
            objects.add(value.accept(this))
        }

        obj.put("known_objects", objects)

        return obj
    }

    override fun visit(model: GroupModel): JSONObject {

        val obj = JSONObject()

        obj.put("type", "Group")

        obj.put("member_type", modelOrRef(model.memberModelType))

        obj.put("number", model.number)

        return obj
    }

    private fun modelOrRef(model: Model): Any {
        return if (knowledge.isKnownObject(model)) model.name else model.accept(this)
    }

    override fun visit(model: AnyModel): JSONObject {
        val obj = JSONObject()

        obj.put("type", "Any")

        val options = JSONArray()

        for (option in model.options) {
            options.add(modelOrRef(option))
        }

        obj.put("options", options)

        return obj

    }

    override fun visit(model: CompositeModel): JSONObject {
        val obj = JSONObject()

        obj.put("type", "Composite")

        val components = JSONArray()

        for (comp in model.components) {
            val component = JSONObject()
            component.put("model", modelOrRef(comp.model!!))
        }

        obj.put("components", components)

        return obj
    }

    override fun visit(model: PrimitiveModel): JSONObject {
        val obj = JSONObject()

        obj.put("type", "Primitive")

        obj.put("shape", model.shape.toString())

        return obj
    }

    override fun visit(model: PlaceholderModel): JSONObject {
        val obj = JSONObject()

        obj.put("type", "Placeholder")

        return obj
    }
}
