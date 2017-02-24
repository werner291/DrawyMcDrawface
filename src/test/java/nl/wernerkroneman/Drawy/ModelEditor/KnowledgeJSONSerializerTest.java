package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.Distance;
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint;
import org.json.simple.JSONObject;
import org.junit.Assert;
import org.junit.Test;

import static nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.ABOVE;

public class KnowledgeJSONSerializerTest {

    @Test
    public void testSimple() {

        Knowledge knowledge = Knowledge.Companion.knowledgeWithPrimitives();

        CompositeModel composite = new CompositeModel("cube above sphere");

        CompositeModel.Component cube = composite.addComponentForModel(
                knowledge.getObject("Cube"));
        CompositeModel.Component sphere = composite.addComponentForModel(
                knowledge.getObject("Sphere"));
        composite.addConstraint(new RelativePositionConstraint(cube,sphere,ABOVE, Distance.ANY));

        KnowledgeJSONSerializer serializer = new KnowledgeJSONSerializer(knowledge);

        JSONObject jsonObject = serializer.serializeKnowledge();

        System.out.println(jsonObject.toJSONString());

        Knowledge reconstructed = KnowledgeJSONDeserializer.INSTANCE.deserializeJSON(jsonObject);

        //Assert.assertTrue(KnowledgeIntegrityChecker.checkIntegrity(reconstructed));

    }
}
