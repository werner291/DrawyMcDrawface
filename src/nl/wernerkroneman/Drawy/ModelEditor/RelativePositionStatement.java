package nl.wernerkroneman.Drawy.ModelEditor;


import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint;

import static nl.wernerkroneman.Drawy.ModelEditor.RelativePositionStatement.RelativePosition.ABOVE;

/**
 * Represents a statement that creates a constraint
 * that results in the result of one "create" command
 * being in a certain relative position with respect to
 * the result of another command.
 */
public class RelativePositionStatement extends SceneCommand {

    private final CreateEntityCommand a;
    private final CreateEntityCommand b;
    private final RelativePosition pos;

    public RelativePositionStatement(CreateEntityCommand a, CreateEntityCommand b, RelativePosition pos,
                                     CompositeModel scene) {
        super(scene);
        this.a = a;
        this.b = b;
        this.pos = pos;
    }

    public CreateEntityCommand getA() {
        return a;
    }

    public CreateEntityCommand getB() {
        return b;
    }

    public RelativePosition getPos() {
        return pos;
    }

    @Override
    void onApply() {
        scene.addConstraint(new RelativePositionConstraint(a.getCreated(), b.getCreated(), ABOVE));
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
