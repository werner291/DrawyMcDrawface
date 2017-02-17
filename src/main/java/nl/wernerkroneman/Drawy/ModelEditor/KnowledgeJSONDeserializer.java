package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class KnowledgeJSONDeserializer {

    static Knowledge deserializeJSON(JSONObject object) {

        Knowledge knowledge = new Knowledge();

        for (Object modelJSON: (JSONArray) object.get("known_objects")) {

            JSONObject serialized = (JSONObject) modelJSON;

            deserializeObject(knowledge, serialized);

        }

        return knowledge;
    }

    private static Model deserializeObject(Knowledge knowledge, Object serialized) {

        if (serialized instanceof String) {
            Model model = knowledge.getObject((String) serialized);

            if (model == null) {
                throw new IllegalStateException("Unresolvable reference: " + (String) serialized);
            } else {
                return model;
            }

        } else if (serialized instanceof JSONObject) {

            JSONObject modelJSON = (JSONObject) serialized;

            if (modelJSON.get("type").equals("Group")) {

                return new GroupModel((Integer) modelJSON.get("number"),
                        deserializeObject(knowledge, modelJSON.get("member_type")), (Integer) modelJSON.get("number") + " x " + deserializeObject(knowledge, modelJSON.get("member_type")).getName());

            } else if (modelJSON.get("type").equals("Composite")) {

                CompositeModel compositeModel = new CompositeModel((String) modelJSON.get("name"));

                for (Object obj: (JSONArray) modelJSON.get("components")) {
                    JSONObject componentJSON = (JSONObject) obj;

                    compositeModel.addComponentForModel(
                            deserializeObject(knowledge, componentJSON.get("model")));
                }

                return compositeModel;

            } else if (modelJSON.get("type").equals("Any")) {

                AnyModel anyModel = new AnyModel((String) modelJSON.get("name"));

                for (Object obj: (JSONArray) modelJSON.get("options")) {
                    JSONObject optionJSON = (JSONObject) obj;

                    anyModel.addOption(deserializeObject(knowledge, optionJSON));
                }

                return anyModel;

            } else if (modelJSON.get("type").equals("Primitive")) {

                PrimitiveModel primitiveModel = new PrimitiveModel(
                        PrimitiveModel.ShapeType.valueOf((String) modelJSON.get("shape")),
                        (String) modelJSON.get("name"));

            } else if (modelJSON.get("type").equals("Placeholder")) {
                return new PlaceholderModel((String) modelJSON.get("name"));
            } else {
                throw new RuntimeException("Unrecognized type " + modelJSON.get("type"));
            }

        } else {
            return null;
        }

        return null;
    }

}
