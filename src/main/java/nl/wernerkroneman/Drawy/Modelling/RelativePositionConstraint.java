package nl.wernerkroneman.Drawy.Modelling;

import static nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.RelativePosition.DimensionOrder.*;

public class RelativePositionConstraint extends Constraint {
    public RelativeConstraintContext.Positionable a, b;
    public RelativePosition pos;
    public Distance dist = Distance.ANY;

    public RelativePositionConstraint() {
    }

    public RelativePositionConstraint(RelativeConstraintContext.Positionable a,
                                      RelativeConstraintContext.Positionable b,
                                      RelativePosition pos,
                                      Distance dist) {
        this.a = a;
        this.b = b;
        this.pos = pos;
        this.dist = dist;
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

    public RelativePosition getPos() {
        return pos;
    }

    public void setPos(RelativePosition pos) {
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

    public static class RelativePosition {

        public enum DimensionOrder {
            BEFORE,AFTER,SAME
        }

        public RelativePosition(DimensionOrder xRel, DimensionOrder yRel, DimensionOrder zRel) {
            this.rel = new DimensionOrder[]{xRel,yRel,zRel};
        }

        public DimensionOrder[] rel;
    }

    public static RelativePosition ABOVE =  new RelativePosition(SAME,  AFTER,  SAME);
    public static RelativePosition BELOW =  new RelativePosition(SAME,  BEFORE, SAME);
    public static RelativePosition FRONT =  new RelativePosition(SAME,  SAME,   AFTER);
    public static RelativePosition BEHIND = new RelativePosition(SAME,  SAME,   BEFORE);
    public static RelativePosition LEFT =   new RelativePosition(BEFORE,SAME,   SAME);
    public static RelativePosition RIGHT =  new RelativePosition(AFTER, SAME,   SAME);
}
