package nl.wernerkroneman.Drawy.Modelling;

import nl.wernerkroneman.Drawy.Modelling.Model;

public class PrimitiveModel extends Model {

    public enum ShapeType {
        CUBE, SPHERE, CYLINDER
    }

    // A bit unelegant right now, will eventually replace with something mesh-based
    // where something class-based actually makes sense.

    ShapeType shape;

    public PrimitiveModel(ShapeType shape, String name) {
        super(name);
        this.shape = shape;
    }
}
