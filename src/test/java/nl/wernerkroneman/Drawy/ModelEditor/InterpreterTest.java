package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.util.List;

import static nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.*;

public class InterpreterTest {

    Interpreter getInterpreter(CompositeModel model) {
        Knowledge knowledge = Knowledge.knowledgeWithPrimitives();

        return new Interpreter(new KnowledgeResolver(knowledge, null, null), null, null, new PrepositionInterpreter(StandardPhraseMatchers.relativePositionPhrases()));
    }

    @Test
    public void interpreterTest1() {

        CompositeModel scene = new CompositeModel("Scene");
        List<EditorCommand> result = getInterpreter(scene).interpret("Create a cube.", scene);

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmt = (CreateEntityEditorCommand) result.get(0);

        Assert.assertTrue(stmt.what instanceof PrimitiveModel);

        Assert.assertEquals("Cube", stmt.what.getName());

    }

    @Test
    @Ignore("Non-deterministic numbers not yet supported")
    public void interpreterTest2() {

        CompositeModel scene = new CompositeModel("Scene");

        List<EditorCommand> result = getInterpreter(scene).interpret("Add a cylinder or two.", scene);

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmt = (CreateEntityEditorCommand) result.get(0);

        Assert.assertTrue(stmt.what instanceof GroupModel);

        Assert.assertEquals(1, ((GroupModel) stmt.what).getNumber());

        Assert.assertEquals("Cylinder", ((GroupModel) stmt.what).getMemberModelType().getName());

    }

    @Test
    public void createSphereAboveCube() {

        CompositeModel scene = new CompositeModel("Scene");

        List<EditorCommand> result = getInterpreter(scene).interpret("A cube above a sphere", scene);

        Assert.assertEquals(1, result.size());

        // ----------------------

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmtA = (CreateEntityEditorCommand) result.get(0);

        Assert.assertTrue(stmtA.what instanceof CompositeModel);

        Assert.assertTrue(((CompositeModel) stmtA.what).getComponents().stream()
                .allMatch(a -> a.getModel() instanceof PrimitiveModel));

        Assert.assertEquals(1, ((CompositeModel) stmtA.what).getConstraints().size());

        Constraint constraint = ((CompositeModel) stmtA.what).getConstraints().iterator().next();
        Assert.assertTrue(constraint instanceof RelativePositionConstraint);

        Assert.assertEquals(ABOVE, ((RelativePositionConstraint)constraint).pos);

        Assert.assertEquals(Distance.ANY, ((RelativePositionConstraint)constraint).dist);

        stmtA.apply();

        // ----------------------

        Assert.assertEquals(1, scene.getComponents().size());

    }

    @Test
    public void createSphereAboveSphereWithDistance() {

        CompositeModel scene = new CompositeModel("Scene");

        List<EditorCommand> result = getInterpreter(scene).interpret("A cube 5 units above a sphere", scene);

        Assert.assertEquals(1, result.size());

        // ----------------------

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmtA = (CreateEntityEditorCommand) result.get(0);

        Assert.assertTrue(stmtA.what instanceof CompositeModel);

        Assert.assertTrue(((CompositeModel) stmtA.what).getComponents().stream()
                .allMatch(a -> a.getModel() instanceof PrimitiveModel));

        Assert.assertEquals(1, ((CompositeModel) stmtA.what).getConstraints().size());

        Constraint constraint = ((CompositeModel) stmtA.what).getConstraints().iterator().next();
        Assert.assertTrue(constraint instanceof RelativePositionConstraint);

        RelativePositionConstraint positionConstraint = (RelativePositionConstraint) constraint;
        Assert.assertEquals(ABOVE, positionConstraint.pos);

        Assert.assertTrue(positionConstraint.dist instanceof FixedDistance);
        Assert.assertEquals(5, ((FixedDistance)positionConstraint.dist).distance, 0.01);

        stmtA.apply();

        // ----------------------

        Assert.assertEquals(1, scene.getComponents().size());

    }

    @Test
    public void createSphereStack() {

        CompositeModel scene = new CompositeModel("Scene");

        List<EditorCommand> result = getInterpreter(scene).interpret("500 cubes above each other", scene);

        Assert.assertEquals(1, result.size());

        // ----------------------

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmtA = (CreateEntityEditorCommand) result.get(0);

        Assert.assertTrue(stmtA.what instanceof GroupModel);

        Assert.assertTrue(((GroupModel) stmtA.what).getMemberModelType() instanceof PrimitiveModel);

        Assert.assertEquals(500, ((GroupModel) stmtA.what).getNumber());

        stmtA.apply();

    }
}