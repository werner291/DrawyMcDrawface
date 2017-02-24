package nl.wernerkroneman.Drawy.ModelEditor.Interpreters

import nl.wernerkroneman.Drawy.ModelEditor.CreateEntityEditorCommand
import nl.wernerkroneman.Drawy.ModelEditor.MainInterpreter
import nl.wernerkroneman.Drawy.Modelling.Model
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree
import java.util.Stack

/**
 * Generate a creation command for the object(s) described in this sentence part.* @param scene A supplier that provides a scene when executed in which to create the object
 */
class CreateCommandInterpreter(internal var subtreeInterpreter: MainInterpreter) : MainInterpreter.InterpretedObjectFactory<CreateEntityEditorCommand> {

    override val interpretedTypePrediction: Class<*>
        get() = CreateEntityEditorCommand::class.java

    override fun createObject(capturings: Map<String, PhraseTree>): CreateEntityEditorCommand? {
        // Allocate a new command, TODO supply the scene somehow
        val createStmt = CreateEntityEditorCommand({null})

        val phraseTree = capturings["what"]!!

        createStmt.what = subtreeInterpreter.interpret(phraseTree,
                {entry : MainInterpreter.InterpreterEntry ->
            Model::class.java.isAssignableFrom(entry.objectFactory.interpretedTypePrediction)
        }) as Model?

        return createStmt
    }
}
