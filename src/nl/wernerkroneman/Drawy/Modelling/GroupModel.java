package nl.wernerkroneman.Drawy.Modelling;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Represents a group (usually understood as a cluster)
 * of a certain number of copies of a certain model.
 *
 * This model is very similar to a {@link CompositeModel},
 * except that the contents are understood to be identical
 * (but they may be interpreted differently individually if
 * non-determinisitc), and the number of components may also
 * vary.
 *
 * It is also a {@link RelativeConstraintContext}, in the sense
 * that you can specify a relation between different elements,
 * usually between one element and the next.
 */
public class GroupModel extends Model implements RelativeConstraintContext {

    // Placeholder values for relative constraints
    // TODO make a proper type
    public static final Positionable PLACEHOLDER_A = new CompositeModel.Component(null);
    public static final CompositeModel.Component PLACEHOLDER_B = new CompositeModel.Component(null);

    int number;

    Model memberType;
    private Collection<Constraint> constraints = new ArrayList<>();

    public GroupModel(int number, Model memberType) {
        super(number + " x " + memberType.getName());

        this.number = number;
        this.memberType = memberType;
    }

    public int getNumber() {
        return number;
    }

    public Model getMemberModelType() {
        return memberType;
    }

    @Override
    public Collection<Constraint> getConstraints() {
        return constraints;
    }
}
