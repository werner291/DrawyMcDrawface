package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.PrimitiveModel;
import nl.wernerkroneman.Drawy.Modelling.Model;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Map;
import java.util.TreeMap;

public class Knowledge {

    // @inv For each entry: model name matches key
    Map<String, Model> knownObjects = new TreeMap<String, Model>(String.CASE_INSENSITIVE_ORDER);

    public static Knowledge knowledgeWithPrimitives() {
        Knowledge knowledge = new Knowledge();

        knowledge.remember(new PrimitiveModel(PrimitiveModel.ShapeType.CUBE, "Cube"));
        knowledge.remember(new PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder"));
        knowledge.remember(new PrimitiveModel(PrimitiveModel.ShapeType.SPHERE, "Sphere"));

        return knowledge;
    }

    public Model getObject(String name) {
        return knownObjects.get(name);
    }

    public void remember(Model model) {
        knownObjects.put(model.getName(), model);
    }

    public int getNumberOfObjects() {
        return knownObjects.size();
    }

    public boolean isKnownObject(Model model) {
        return knownObjects.values().stream().anyMatch(kO -> model.equals(kO));
    }

}