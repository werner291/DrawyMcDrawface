package nl.wernerkroneman.Drawy.Modelling;

import java.io.Serializable;

public class Model implements Serializable {

  public String name;

    Model(String name) {
      this.name = name;
    }

  public String getName()
  {
    return name;
  }
}
