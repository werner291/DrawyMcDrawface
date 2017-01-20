package nl.wernerkroneman.Drawy.Modelling;

import java.io.Serializable;

public abstract class Model implements Serializable {

    public String name;

    Model(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public abstract <V extends Object> V accept(ModelVisitor<V> visitor);

}
