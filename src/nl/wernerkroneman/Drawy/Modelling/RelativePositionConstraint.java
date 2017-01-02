package nl.wernerkroneman.Drawy.Modelling;

import nl.wernerkroneman.Drawy.ModelEditor.RelativePositionStatement;

public class RelativePositionConstraint extends Constraint {
    RelativeConstraintContext.Positionable a, b;
    RelativePositionStatement.RelativePosition pos;

    public RelativePositionConstraint(RelativeConstraintContext.Positionable a,
                                      RelativeConstraintContext.Positionable b,
                                      RelativePositionStatement.RelativePosition pos) {
        this.a = a;
        this.b = b;
        this.pos = pos;
    }

    public RelativeConstraintContext.Positionable getA() {
        return a;
    }

    public void setA(CompositeModel.Component a) {
        this.a = a;
    }

    public RelativeConstraintContext.Positionable getB() {
        return b;
    }

    public void setB(CompositeModel.Component b) {
        this.b = b;
    }

    public RelativePositionStatement.RelativePosition getPos() {
        return pos;
    }

    public void setPos(RelativePositionStatement.RelativePosition pos) {
        this.pos = pos;
    }

    @Override
    public String toString() {
        return "RelativePositionConstraint{" +
                "a=" + a +
                ", b=" + b +
                ", pos=" + pos +
                '}';
    }
}
