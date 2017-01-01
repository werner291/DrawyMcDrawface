package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.GroupModel;
import nl.wernerkroneman.Drawy.Modelling.Model;

public class CreateEntityCommand extends SceneCommand {

    // String describing what to create
    Model what;

    // AfteModelr applying, stores the entity that was created
    CompositeModel.Component created;

    // How many entities to create
    int number = 1;

    CreateEntityCommand(CompositeModel scene) {
        super(scene);
    }

    public String toString() {
        return "Create " + number + " " + what;
    }

    @Override
    void onApply(){
        created = scene.addComponentForModel(number == 1 ? what : new GroupModel(number, what));
    }

    @Override
    void onRevert() {
        scene.getComponents().remove(created);
    }

    public CompositeModel.Component getCreated() {
        return created;
    }
}
