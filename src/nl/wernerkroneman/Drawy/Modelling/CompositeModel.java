package nl.wernerkroneman.Drawy.Modelling;

import java.util.ArrayList;
import java.util.List;

/**
 * The CompositeModel represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 * 
 * It describes the scene on a very high level, and deals
 * in constraints rather than realisations of those constraints.
 */
public class CompositeModel extends Model {

    int idGen = 0;
    List<ModelInstance> entities = new ArrayList<>();

    public CompositeModel(String name) {
        super(name);
    }

    /**
     * Obtain a list of model instances in this model.
     * DO NOT MODIFY
     *
     * @return A list of model instances.
     */
    public List<ModelInstance> getEntities() {
        return entities;
    }

    public ModelInstance createInstance(Model base) {
        return createInstance(base, String.format("%s-%d", base.getName(), idGen++));
    }

    public ModelInstance createInstance(Model base, String name) {
        ModelInstance created = new ModelInstance(base);

        entities.add(created);

        return created;
    }

    public void deleteEntity(ModelInstance toDelete) {
        entities.remove(toDelete);
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();

        for (ModelInstance model : entities) {
            builder.append(model.toString());
        }

        return builder.toString();
    }

    public static class ModelInstance {

        Model base;

        public ModelInstance(Model base) {
            this.base = base;
        }

        public Model getBase() {
            return base;
        }
    }
}