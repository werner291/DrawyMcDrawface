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

object KnowledgeJSONDeserializer {

    fun deserializeJSON(`object`: JSONObject): Knowledge {

        val knowledge = Knowledge()

        for (modelJSON in `object`["known_objects"] as JSONArray) {

            val serialized = modelJSON as JSONObject

            deserializeObject(knowledge, serialized)

        }

        return knowledge
    }

    private fun deserializeObject(knowledge: Knowledge, serialized: Any): Model? {

        if (serialized is String) {
            val model = knowledge.getObject(serialized)

            if (model == null) {
                throw IllegalStateException("Unresolvable reference: " + serialized)
            } else {
                return model
            }

        } else if (serialized is JSONObject) {

            if (serialized["type"] == "Group") {

                if (serialized.get("member_type") == null) {
                    throw RuntimeException("Group without member type.")
                }

                return GroupModel(serialized["number"] as Int,
                        deserializeObject(knowledge, serialized.get("member_type")!!)!!,
                        (serialized["number"] as Int).toString() + " x " + deserializeObject(knowledge, serialized.get("member_type") as Any)!!.name)

            } else if (serialized["type"] == "Composite") {

                val compositeModel = CompositeModel(serialized["name"] as String)

                for (obj in serialized["components"] as JSONArray) {
                    val componentJSON = obj as JSONObject

                    compositeModel.createComponentForModel(
                            deserializeObject(knowledge, componentJSON.get("model") as Any)!!)
                }

                return compositeModel

            } else if (serialized["type"] == "Any") {

                val anyModel = AnyModel(serialized["name"] as String)

                for (obj in serialized["options"] as JSONArray) {
                    val optionJSON = obj as JSONObject

                    anyModel.addOption(deserializeObject(knowledge, optionJSON)!!)
                }

                return anyModel

            } else if (serialized["type"] == "Primitive") {

                val primitiveModel = PrimitiveModel(
                        PrimitiveModel.ShapeType.valueOf(serialized["shape"] as String),
                        serialized["name"] as String)

            } else if (serialized["type"] == "Placeholder") {
                return PlaceholderModel(serialized["name"] as String)
            } else {
                throw RuntimeException("Unrecognized type " + serialized["type"])
            }

        } else {
            return null
        }

        return null
    }

}
