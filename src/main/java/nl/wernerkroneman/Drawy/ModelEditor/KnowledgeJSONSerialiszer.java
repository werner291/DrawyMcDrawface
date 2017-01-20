package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import java.util.Map;
import java.util.stream.Collectors;

public class KnowledgeJSONSerialiszer implements ModelVisitor<JSONObject> {

    JSONObject serializeKnowledge(Knowledge knowledge) {

        JSONObject obj = new JSONObject();

        obj.put("type","Knowledge");

        JSONObject objects = new JSONObject();

        objects.putAll(knowledge.knownObjects.entrySet().stream()
                .collect(Collectors.toMap(entry -> entry.getKey(),
                        entry -> entry.getValue().accept(this))));

        obj.put("known_objects",objects);

        return obj;
    }

    @Override
    public JSONObject visit(GroupModel model) {

        JSONObject obj = new JSONObject();

        obj.put("type", "Group");

        obj.put("member_type", model.getMemberModelType().accept(this));

        obj.put("number", model.getNumber());

        return obj;
    }

    @Override
    public JSONObject visit(AnyModel model) {
        JSONObject obj = new JSONObject();

        obj.put("type", "Any");

        JSONArray options = new JSONArray();

        for (Model option: model.getOptions()){
            options.add(option.accept(this));
        }

        obj.put("options", options);

        return obj;

    }

    @Override
    public JSONObject visit(CompositeModel model) {
        JSONObject obj = new JSONObject();

        obj.put("type", "Composite");

        JSONArray components = new JSONArray();

        for (CompositeModel.Component comp: model.getComponents()){
            JSONObject component = new JSONObject();
            component.put("model",comp.getModel().accept(this));
        }

        obj.put("components", components);

        return obj;
    }

    @Override
    public JSONObject visit(PrimitiveModel model) {
        JSONObject obj = new JSONObject();

        obj.put("type", "Primitive");

        obj.put("shape", model.getShape().toString());

        return obj;
    }

    public Knowledge deserializeKnowledge(JSONObject obj) {

        if (! obj.get("type").equals("Knowledge")) {
            throw new IllegalArgumentException("Object is not a Knowledge");
        }

        JSONObject knownObjects = (JSONObject) obj.get("known_objects");

        Knowledge knowledge = new Knowledge();

        for (Object entryObj: knownObjects.entrySet()) {
            Map.Entry<String,Object> entry = (Map.Entry<String,Object>) entryObj;

            if (knowledge.getObject(entry.getKey()) == null) {
                //deserializeTopLevelObject(knowledge);
            }
        }

        return knowledge;

    }
}
