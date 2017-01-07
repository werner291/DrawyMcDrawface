package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.Distance;
import nl.wernerkroneman.Drawy.Modelling.RelativeConstraintContext;
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint;

import java.util.function.Supplier;

/**
 * Represents a statement that creates a constraint
 * between two components in a CompositeModel.
 */
public class RelativePositionStatement extends EditorCommand {

    private final Supplier<? extends RelativeConstraintContext.Positionable> a;
    private final Supplier<? extends RelativeConstraintContext.Positionable> b;
    private final Supplier<? extends RelativeConstraintContext> target;
    private final RelativePositionConstraint.RelativePosition pos;

    public RelativePositionStatement(Supplier<? extends RelativeConstraintContext.Positionable> a,
                                     Supplier<? extends RelativeConstraintContext.Positionable> b,
                                     RelativePositionConstraint.RelativePosition pos,
                                     Supplier<? extends RelativeConstraintContext> target) {
        this.target = target;
        this.a = a;
        this.b = b;
        this.pos = pos;
    }

    public Supplier<? extends RelativeConstraintContext.Positionable> getA() {
        return a;
    }

    public Supplier<? extends RelativeConstraintContext.Positionable> getB() {
        return b;
    }

    public RelativePositionConstraint.RelativePosition getPos() {
        return pos;
    }

    @Override
    void onApply() {
        target.get().getConstraints().add(new RelativePositionConstraint(a.get(), b.get(), pos, Distance.ANY));
    }

    @Override
    public String toString() {
        return "RelativePositionStatement(" + a + " " + pos + " " + b + ")";
    }

}
