package nl.wernerkroneman.Drawy.Modelling;

/**
 * Class representing a placeholder in a model,
 * something "unresolved".
 *
 * Usually, this model and any model containing it
 * cannot be represented directly.
 *
 * "object" is usually a PlaceholderModel.
 */
public class PlaceholderModel extends Model {

    public PlaceholderModel(String name) {
        super(name);
    }

    @Override
    public <V> V accept(ModelVisitor<V> visitor) {
        return visitor.visit(this);
    }
}
