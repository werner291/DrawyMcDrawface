package nl.wernerkroneman.Drawy.AbstractToConcreteConverter;

import nl.wernerkroneman.Drawy.Algorithms.TopologicalSort;
import nl.wernerkroneman.Drawy.ConcreteModelling.SceneNode;
import nl.wernerkroneman.Drawy.Modelling.*;

import java.util.*;

/**
 * A convenience class that allows binding between
 * a group of SceneNodes and abstract models.
 */
public class PositionResolutionContext {

    List<Component> components = new ArrayList<>();

    PositionResolutionContext(GroupModel groupModel) {
        for (int i = 0; i < groupModel.getNumber(); i++) {
            Component component = new Component(groupModel.getMemberModelType());
            components.add(component);

            if (i >= 1) {
                for (Constraint absConstr : groupModel.getConstraints()) {
                    // TODO will have to change for other types of constraints
                    component.constraintRelatedTo.add(new Component.Constraint(components.get(components.size() - 2),
                            (RelativePositionConstraint) absConstr));
                }
            }
        }
    }

    PositionResolutionContext(CompositeModel composite) {

        Map<CompositeModel.Component, Component> absToComp = new HashMap<>();

        for (CompositeModel.Component absComponent : composite.getComponents()) {
            Component component = new Component(absComponent.getModel());
            components.add(component);
            absToComp.put(absComponent, component);
        }

        for (Constraint absConstr : composite.getConstraints()) {
            assert absConstr instanceof RelativePositionConstraint;

            RelativePositionConstraint relConstr = (RelativePositionConstraint) absConstr;

            Component a = absToComp.get(relConstr.getA());
            a.constraintRelatedTo.add(new Component.Constraint(absToComp.get(relConstr.getB()), relConstr));
        }

        topologicalSortComponents();
    }

    public List<Component> getComponents() {
        return components;
    }

    private void topologicalSortComponents() {
        Map<Component, List<Component>> dependencies = new HashMap<>();

        for (Component comp : components) {
            dependencies.put(comp, new ArrayList<>());
        }

        for (Component comp : components) {
            for (Component.Constraint constr : comp.constraintRelatedTo) {
                dependencies.get(comp).add(constr.relativeTo);
            }
        }

        components = TopologicalSort.topologicalSort(components, dependencies);
    }

    static class Component {

        SceneNode node;
        Model abstractModel;
        Collection<Constraint> constraintRelatedTo = new ArrayList<>();

        public Component(Model abstractModel) {
            this.abstractModel = abstractModel;
        }

        static class Constraint {
            Component relativeTo;
            RelativePositionConstraint abstractConstraint;

            public Constraint(Component relativeTo, RelativePositionConstraint abstractConstraint) {
                this.relativeTo = relativeTo;
                this.abstractConstraint = abstractConstraint;
            }
        }
    }

}
