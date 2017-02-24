package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Modelling.Model

/**
 * Created by werner on 30-12-16.
 */
interface ModelResolver {
    fun resolveObject(name: String): Model
}
