package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.ModelEditor.CreateEntityEditorCommand;
import nl.wernerkroneman.Drawy.ModelEditor.MainInterpreter;
import nl.wernerkroneman.Drawy.Modelling.Model;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree;

import java.util.Map;
import java.util.Stack;

/**
 * Generate a creation command for the object(s) described in this sentence part.* @param scene A supplier that provides a scene when executed in which to create the object
 */
public class CreateCommandInterpreter
        implements MainInterpreter.InterpretedObjectFactory<CreateEntityEditorCommand> {

    public CreateCommandInterpreter(MainInterpreter subtreeInterpreter) {
        this.subtreeInterpreter = subtreeInterpreter;
    }

    MainInterpreter subtreeInterpreter;

    @Override
    public Class getInterpretedTypePrediction() {
        return CreateEntityEditorCommand.class;
    }

    @Override
    public CreateEntityEditorCommand createObject(Map<String, PhraseTree> capturings) {
        // Allocate a new command, TODO supply the scene somehow
        CreateEntityEditorCommand createStmt = new CreateEntityEditorCommand(null);

        createStmt.what = (Model) subtreeInterpreter.interpret(capturings.get("what"),
                (e) -> Model.class.isAssignableFrom(e.objectFactory.getInterpretedTypePrediction()));

        return createStmt;
    }
}
