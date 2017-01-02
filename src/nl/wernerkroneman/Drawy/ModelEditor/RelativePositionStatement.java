package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.RelativeConstraintContext;
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint;

import java.util.function.Supplier;

import static nl.wernerkroneman.Drawy.ModelEditor.RelativePositionStatement.RelativePosition.ABOVE;

/**
 * Represents a statement that creates a constraint
 * between two components in a CompositeModel.
 */
public class RelativePositionStatement extends EditorCommand {

    private final Supplier<? extends RelativeConstraintContext.Positionable> a;
    private final Supplier<? extends RelativeConstraintContext.Positionable> b;
    private final Supplier<? extends RelativeConstraintContext> target;
    private final RelativePosition pos;

    public RelativePositionStatement(Supplier<? extends RelativeConstraintContext.Positionable> a,
                                     Supplier<? extends RelativeConstraintContext.Positionable> b,
                                     RelativePosition pos,
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

    public RelativePosition getPos() {
        return pos;
    }

    @Override
    void onApply() {
        target.get().getConstraints().add(new RelativePositionConstraint(a.get(), b.get(), ABOVE));
    }

    @Override
    public String toString() {
        return "RelativePositionStatement{" +
                "a=" + a +
                ", b=" + b +
                ", pos=" + pos +
                '}';
    }

    public enum RelativePosition {
        ABOVE, BELOW
    }
}
