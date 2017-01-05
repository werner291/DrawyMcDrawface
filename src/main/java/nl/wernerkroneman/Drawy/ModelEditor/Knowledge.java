package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.PrimitiveModel;
import nl.wernerkroneman.Drawy.Modelling.Model;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Map;
import java.util.TreeMap;

public class Knowledge implements Serializable {

    Map<String, Model> knownObjects = new TreeMap<String, Model>(String.CASE_INSENSITIVE_ORDER);

    public static Knowledge knowledgeWithPrimitives() {
        Knowledge knowledge = new Knowledge();

        knowledge.remember("Cube", new PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube"));
        knowledge.remember("Cylinder", new PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder"));
        knowledge.remember("Sphere", new PrimitiveModel(PrimitiveModel.ShapeType.SPHERE, "Sphere"));

        return knowledge;
    }

    Model getObject(String name) {
        return knownObjects.get(name);
    }

    void remember(String name, Model model) {
        knownObjects.put(name, model);

        try (FileOutputStream file = new FileOutputStream("knowledge.txt")) {

            ObjectOutputStream out = new ObjectOutputStream(file);

            out.writeObject(this);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public int getNumberOfObjects() {
        return knownObjects.size();
    }

}