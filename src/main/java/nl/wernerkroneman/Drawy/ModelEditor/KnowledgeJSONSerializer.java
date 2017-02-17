package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import java.util.Map;

/**
 * Class that converts a Knowledge into a JSON object.
 *
 * A Knowledge is provided on construction, and serializeKnowledge()
 * is called to obtain a JSONObject that represents the Knowledge.
 */
public class KnowledgeJSONSerializer implements ModelVisitor<JSONObject> {

    Knowledge knowledge;

    public KnowledgeJSONSerializer(Knowledge knowledge) {
        this.knowledge = knowledge;
    }

    JSONObject serializeKnowledge() {

        JSONObject obj = new JSONObject();

        obj.put("type","Knowledge");

        JSONArray objects = new JSONArray();

        for (Map.Entry<String, Model> modelEntry : knowledge.knownObjects.entrySet()) {
            objects.add(modelEntry.getValue().accept(this));
        }

        obj.put("known_objects",objects);

        return obj;
    }

    @Override
    public JSONObject visit(GroupModel model) {

        JSONObject obj = new JSONObject();

        obj.put("type", "Group");

        obj.put("member_type", modelOrRef(model.getMemberModelType()));

        obj.put("number", model.getNumber());

        return obj;
    }

    private Object modelOrRef(Model model) {
        return knowledge.isKnownObject(model) ? model.getName() : model.accept(this);
    }

    @Override
    public JSONObject visit(AnyModel model) {
        JSONObject obj = new JSONObject();

        obj.put("type", "Any");

        JSONArray options = new JSONArray();

        for (Model option: model.getOptions()){
            options.add(modelOrRef(option));
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
            component.put("model",modelOrRef(comp.getModel()));
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

    @Override
    public JSONObject visit(PlaceholderModel model) {
        JSONObject obj = new JSONObject();

        obj.put("type", "Placeholder");

        return obj;
    }
}
