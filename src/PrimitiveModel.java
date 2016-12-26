public class PrimitiveModel extends Model {

    enum ShapeType {
        CUBE, SPHERE, CYLINDER
    }

    // A bit unelegant right now, will eventually replace with something mesh-based
    // where something class-based actually makes sense.

    ShapeType shape;

    PrimitiveModel(ShapeType shape, String name) {
        super(name);
        this.shape = shape;
    }
}
