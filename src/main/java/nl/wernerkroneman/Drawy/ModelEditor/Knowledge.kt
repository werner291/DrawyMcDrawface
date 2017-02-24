package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Modelling.PrimitiveModel
import nl.wernerkroneman.Drawy.Modelling.Model

import java.io.FileOutputStream
import java.io.IOException
import java.io.ObjectOutputStream
import java.io.Serializable
import java.util.TreeMap

class Knowledge {

    // @inv For each entry: model name matches key
    var knownObjects: MutableMap<String, Model> = TreeMap(String.CASE_INSENSITIVE_ORDER)

    fun getObject(name: String): Model? {
        return knownObjects[name]
    }

    fun remember(model: Model) {
        knownObjects.put(model.getName(), model)
    }

    val numberOfObjects: Int
        get() = knownObjects.size

    fun isKnownObject(model: Model): Boolean {
        return knownObjects.values.any({ it == model })
    }

    companion object {

        fun knowledgeWithPrimitives(): Knowledge {
            val knowledge = Knowledge()

            knowledge.remember(PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube"))
            knowledge.remember(PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder"))
            knowledge.remember(PrimitiveModel(PrimitiveModel.ShapeType.SPHERE, "Sphere"))

            return knowledge
        }
    }

}