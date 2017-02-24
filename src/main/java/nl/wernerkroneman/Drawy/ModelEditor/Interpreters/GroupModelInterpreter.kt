package nl.wernerkroneman.Drawy.ModelEditor.Interpreters

import nl.wernerkroneman.Drawy.ModelEditor.Knowledge
import nl.wernerkroneman.Drawy.ModelEditor.MainInterpreter
import nl.wernerkroneman.Drawy.Modelling.Model
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree

/**
 * Interpreter for Model.
 */
class GroupModelInterpreter(internal var subtreeInterpreter: MainInterpreter,
                            private val knowledge: Knowledge) : MainInterpreter.InterpretedObjectFactory<Model> {

    override val interpretedTypePrediction: Class<*>
        get() = Model::class.java

    override fun createObject(capturings: Map<String, PhraseTree>): Model? {

        // Try retrieving the object by name
        val objectName = capturings["object name"]!!.getRootWord()
        val model = knowledge.getObject(objectName)

        return model
    }
}
