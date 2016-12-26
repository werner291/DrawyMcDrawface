import java.io.Serializable;

public class Model implements Serializable {

  String name;

    Model(String name) {
      this.name = name;
    }

  String getName()
  {
    return name;
  }
}
