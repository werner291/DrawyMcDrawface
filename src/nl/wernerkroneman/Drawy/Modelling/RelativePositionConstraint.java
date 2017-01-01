package nl.wernerkroneman.Drawy.Modelling;

import nl.wernerkroneman.Drawy.ModelEditor.RelativePositionStatement;

public class RelativePositionConstraint extends Constraint {
    CompositeModel.Component a, b;
    RelativePositionStatement.RelativePosition pos;

    public RelativePositionConstraint(CompositeModel.Component a, CompositeModel.Component b,
                                      RelativePositionStatement.RelativePosition pos) {
        this.a = a;
        this.b = b;
        this.pos = pos;
    }

    public CompositeModel.Component getA() {
        return a;
    }

    public void setA(CompositeModel.Component a) {
        this.a = a;
    }

    public CompositeModel.Component getB() {
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
