package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.ModelEditor.CreateEntityEditorCommand;
import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import org.junit.Test;

public class EditorCommandTest {



    @Test(expected = RuntimeException.class)
    public void creationDouble() {

        CompositeModel model = new CompositeModel("Scene");

        CreateEntityEditorCommand stmt = new CreateEntityEditorCommand(() -> model);

        stmt.apply();
        stmt.apply();

    }

}