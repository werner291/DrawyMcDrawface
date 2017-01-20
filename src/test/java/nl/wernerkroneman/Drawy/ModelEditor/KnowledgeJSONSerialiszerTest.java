package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.Distance;
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint;
import org.json.simple.JSONObject;
import org.junit.Test;

import java.util.Map;

import static nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.ABOVE;

public class KnowledgeJSONSerialiszerTest {

    @Test
    public void testSimple() {

        Knowledge knowledge = Knowledge.knowledgeWithPrimitives();

        CompositeModel composite = new CompositeModel("cube above sphere");

        CompositeModel.Component cube = composite.addComponentForModel(
                knowledge.getObject("Cube"));
        CompositeModel.Component sphere = composite.addComponentForModel(
                knowledge.getObject("Sphere"));
        composite.addConstraint(new RelativePositionConstraint(cube,sphere,ABOVE, Distance.ANY));

        KnowledgeJSONSerialiszer serializer = new KnowledgeJSONSerialiszer();

        JSONObject jsonObject = serializer.serializeKnowledge(knowledge);

        System.out.println(jsonObject.toJSONString());

        Knowledge reconstructed = serializer.deserializeKnowledge(jsonObject);



    }
}
