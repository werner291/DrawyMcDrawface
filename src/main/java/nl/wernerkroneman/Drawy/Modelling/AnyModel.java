package nl.wernerkroneman.Drawy.Modelling;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class AnyModel extends Model {

    static Random random = new Random();

    List<Model> options = new ArrayList();

    public AnyModel(String name) {
        super(name);
    }

    Model getAny() {
        return options.get(random.nextInt(options.size()));
    }

    @Override
    public String toString() {
        return "AnyModel{" +
                "options=" + options +
                '}';
    }

    public void addOption(Model newSubjectDef) {
        options.add(newSubjectDef);
    }
}