import java.util.ArrayList;
import java.util.List;

public class CreateEntityRule extends SceneCommand {


    // String describing what to create
    Model what;

    // After applying, stores the entity that was created
    List<CompositeModel.ModelInstance> created;

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
        created = new ArrayList<>();
        created.add(scene.createInstance(what));
    }

    @Override
    void onRevert() {
        for (CompositeModel.ModelInstance instance : created) {
            scene.deleteEntity(instance);
            created = null;
        }
    }


}
