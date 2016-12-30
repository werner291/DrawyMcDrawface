package nl.wernerkroneman.Drawy.Modelling;

/**
 * Represents a group (usually understood as a cluster)
 * of a certain number of copies of a certain model.
 */
public class GroupModel extends Model {

    int number;

    Model memberType;

    public GroupModel(int number, Model memberType) {
        super(number + " x " + memberType.getName());

        this.number = number;
        this.memberType = memberType;
    }

    public int getNumber() {
        return number;
    }

    public Model getMember() {
        return memberType;
    }
}
