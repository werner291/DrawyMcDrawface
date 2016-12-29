package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.GroupModel;
import nl.wernerkroneman.Drawy.Modelling.Model;

public class CreateEntityRule extends SceneCommand {


    // String describing what to create
    Model what;

    // AfteModelr applying, stores the entity that was created
    GroupModel created;

    // How many entities to create
    int number = 1;

    CreateEntityRule(CompositeModel scene) {
        super(scene);
    }

    public String toString() {
        return "Create " + number + " " + what;
    }

    @Override
    void onApply(){
        created = new GroupModel(number, what);
        scene.getComponents().add(created);
    }

    @Override
    void onRevert() {
        scene.getComponents().remove(created);
    }


}
