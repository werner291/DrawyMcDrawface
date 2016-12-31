package nl.wernerkroneman.Drawy.Modelling;

import java.util.HashSet;
import java.util.Set;

/**
 * The CompositeModel represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 * 
 * It describes the scene on a very high level, and deals
 * in constraints rather than realisations of those constraints.
 */
public class CompositeModel extends Model {

    Set<Component> components = new HashSet<>();

    public CompositeModel(String name) {
        super(name);
    }

    public void addComponentForModel(PrimitiveModel cube) {
        components.add(new Component(cube));
    }

    /**
     * Obtain a list of model instances in this model.
     *
     * @return A list of model instances.
     */
    public Set<Component> getComponents() {
        return components;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();

        for (Component comp : components) {
            builder.append(comp.toString());
        }

        return builder.toString();
    }

    public static class Component {
        private Model model;

        public Component(Model model) {
            this.model = model;
        }

        public Model getModel() {
            return model;
        }
    }
}