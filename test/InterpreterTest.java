import org.junit.Assert;
import org.junit.Test;

import java.util.List;

public class InterpreterTest {

    Interpreter getInterpreter(CompositeModel model) {
        Knowledge knowledge = Knowledge.knowledgeWithPrimitives();

        return new Interpreter(new Resolver(null, knowledge) {
            @Override
            void askUserForObject(String name) {
                // Should not ask for clarification
                Assert.fail();
            }
        });
    }

    @Test
    public void interpreterTest1() {

        CompositeModel scene = new CompositeModel("Scene");
        List<SceneCommand> result = getInterpreter(scene).interpret("Create a cube.", scene);

        Assert.assertTrue(result.get(0) instanceof CreateEntityRule);

        CreateEntityRule stmt = (CreateEntityRule) result.get(0);

        Assert.assertEquals(1, stmt.number);

        Assert.assertEquals("Cube", stmt.what.getName());

    }

    @Test
    public void interpreterTest2() {

        CompositeModel scene = new CompositeModel("Scene");

        List<SceneCommand> result = getInterpreter(scene).interpret("Add a palm tree or two.", scene);

        Assert.assertTrue(result.get(0) instanceof CreateEntityRule);

        CreateEntityRule stmt = (CreateEntityRule) result.get(0);

        Assert.assertTrue(1 <= stmt.number);
        Assert.assertTrue(2 >= stmt.number);

        Assert.assertEquals("tree", stmt.what.getName());

    }

    @Test
    public void interpreterTestNumeric() {

        CompositeModel scene = new CompositeModel("Scene");

        List<SceneCommand> result = getInterpreter(scene).interpret("50 spheres", scene);

        Assert.assertTrue(result.get(0) instanceof CreateEntityRule);

        CreateEntityRule stmt = (CreateEntityRule) result.get(0);

        Assert.assertEquals(50, stmt.number);

        Assert.assertEquals("Sphere", stmt.what.getName());

    }

    @Test
    public void createWithAnd() {

        CompositeModel scene = new CompositeModel("Scene");

        List<SceneCommand> result = getInterpreter(scene).interpret("Add a cube and a cylinder", scene);

        Assert.assertEquals(2, result.size());


        CreateEntityRule stmt = (CreateEntityRule) result.get(0);

        Assert.assertTrue(result.get(0) instanceof CreateEntityRule);

        Assert.assertEquals(1, stmt.number);

        Assert.assertEquals("Cube", stmt.what.getName());


        Assert.assertTrue(result.get(1) instanceof CreateEntityRule);

        stmt = (CreateEntityRule) result.get(1);

        Assert.assertEquals(1, stmt.number);

        Assert.assertEquals("Cylinder", stmt.what.getName());

    }
}