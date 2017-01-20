package nl.wernerkroneman.Drawy.Modelling;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * The CompositeModel represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 * <p>
 * It describes the scene on a very high level, and deals
 * in constraints rather than realisations of those constraints.
 */
public class CompositeModel extends Model implements RelativeConstraintContext {

    private Set<Component> components = new HashSet<>();
    private Set<Constraint> constraints = new HashSet<>();

    public CompositeModel(String name) {
        super(name);
    }

    @Override
    public <V> V accept(ModelVisitor<V> visitor) {
        return visitor.visit(this);
    }

    public Component addComponentForModel(Model cube) {
        Component comp = new Component(cube);
        components.add(comp);
        return comp;
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

        for (Component comp : getComponents()) {
            builder.append(comp.toString());
        }

        return builder.toString();
    }

    @Override
    public Collection<Constraint> getConstraints() {
        return constraints;
    }

    public void addConstraint(RelativePositionConstraint relativePositionConstraint) {
        constraints.add(relativePositionConstraint);
    }

    public static class Component implements Positionable {
        private Model model;

        public Component(Model model) {
            this.model = model;
        }

        public Model getModel() {
            return model;
        }
    }
}