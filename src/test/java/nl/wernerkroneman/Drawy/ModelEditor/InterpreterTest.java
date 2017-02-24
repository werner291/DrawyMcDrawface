package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.util.List;

import static nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.*;

public class InterpreterTest {

    MainInterpreter getInterpreter(CompositeModel model) {
        Knowledge knowledge = Knowledge.Companion.knowledgeWithPrimitives();

        return new MainInterpreter();
    }

    @Test
    public void interpreterTest1() {

        CompositeModel scene = new CompositeModel("Scene");
        EditorCommand result = getInterpreter(scene).interpret("Create a cube.", scene);

        Assert.assertTrue(result instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmt = (CreateEntityEditorCommand) result;

        Assert.assertTrue(stmt.getWhat() instanceof PrimitiveModel);

        Assert.assertEquals("Cube", stmt.getWhat().getName());

    }

    @Test
    @Ignore("Non-deterministic numbers not yet supported")
    public void interpreterTest2() {

        CompositeModel scene = new CompositeModel("Scene");

        EditorCommand result = getInterpreter(scene).interpret("Add a cylinder or two.", scene);

        Assert.assertTrue(result instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmt = (CreateEntityEditorCommand) result;

        Assert.assertTrue(stmt.getWhat() instanceof GroupModel);

        Assert.assertEquals(1, ((GroupModel) stmt.getWhat()).getNumber());

        Assert.assertEquals("Cylinder", ((GroupModel) stmt.getWhat()).getMemberModelType().getName());

    }

    @Test
    public void createSphereAboveCube() {

        CompositeModel scene = new CompositeModel("Scene");

        EditorCommand result = getInterpreter(scene).interpret("A cube above a sphere", scene);

        Assert.assertNotNull(result);

        // ----------------------

        Assert.assertTrue(result instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmtA = (CreateEntityEditorCommand) result;

        Assert.assertTrue(stmtA.getWhat() instanceof CompositeModel);

        Assert.assertTrue(((CompositeModel) stmtA.getWhat()).getComponents().stream()
                .allMatch(a -> a.getModel() instanceof PrimitiveModel));

        Assert.assertEquals(1, ((CompositeModel) stmtA.getWhat()).getConstraints().size());

        Constraint constraint = ((CompositeModel) stmtA.getWhat()).getConstraints().iterator().next();
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

        EditorCommand result = getInterpreter(scene).interpret("A cube 5 units above a sphere", scene);

        Assert.assertNotNull(result);

        // ----------------------

        Assert.assertTrue(result instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmtA = (CreateEntityEditorCommand) result;

        Assert.assertTrue(stmtA.getWhat() instanceof CompositeModel);

        Assert.assertTrue(((CompositeModel) stmtA.getWhat()).getComponents().stream()
                .allMatch(a -> a.getModel() instanceof PrimitiveModel));

        Assert.assertEquals(1, ((CompositeModel) stmtA.getWhat()).getConstraints().size());

        Constraint constraint = ((CompositeModel) stmtA.getWhat()).getConstraints().iterator().next();
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

        EditorCommand result = getInterpreter(scene).interpret("500 cubes above each other", scene);

        Assert.assertNotNull(result);

        // ----------------------

        Assert.assertTrue(result instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmtA = (CreateEntityEditorCommand) result;

        Assert.assertTrue(stmtA.getWhat() instanceof GroupModel);

        Assert.assertTrue(((GroupModel) stmtA.getWhat()).getMemberModelType() instanceof PrimitiveModel);

        Assert.assertEquals(500, ((GroupModel) stmtA.getWhat()).getNumber());

        stmtA.apply();

    }
}