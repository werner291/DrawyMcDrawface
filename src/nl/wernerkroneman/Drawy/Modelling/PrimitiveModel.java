package nl.wernerkroneman.Drawy.Modelling;

public class PrimitiveModel extends Model {

    ShapeType shape;

    // A bit unelegant right now, will eventually replace with something mesh-based
    // where something class-based actually makes sense.

    public PrimitiveModel(ShapeType shape, String name) {
        super(name);
        this.shape = shape;
    }

    @Override
    public String toString() {
        return "Primitive " + shape;
    }

    public enum ShapeType {
        CUBE, SPHERE, CYLINDER
    }
}
