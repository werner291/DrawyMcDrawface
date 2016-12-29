package nl.wernerkroneman.Drawy.Modelling;

import java.util.ArrayList;
import java.util.List;

/**
 * The CompositeModel represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 * 
 * It describes the scene on a very high level, and deals
 * in constraints rather than realisations of those constraints.
 */
public class CompositeModel extends Model {

    List<Model> components = new ArrayList<>();

    public CompositeModel(String name) {
        super(name);
    }

    /**
     * Obtain a list of model instances in this model.
     * DO NOT MODIFY
     *
     * @return A list of model instances.
     */
    public List<Model> getComponents() {
        return components;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();

        for (Model model : components) {
            builder.append(model.toString());
        }

        return builder.toString();
    }
}