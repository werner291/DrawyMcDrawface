package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

import static nl.wernerkroneman.Drawy.ModelEditor.RelativePositionStatement.RelativePosition.ABOVE;

public class InterpreterTest {

    Interpreter getInterpreter(CompositeModel model) {
        Knowledge knowledge = Knowledge.knowledgeWithPrimitives();

        return new Interpreter(new KnowledgeResolver(knowledge, null, null));
    }

    @Test
    public void interpreterTest1() {

        CompositeModel scene = new CompositeModel("Scene");
        List<EditorCommand> result = getInterpreter(scene).interpret("Create a cube.", scene);

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmt = (CreateEntityEditorCommand) result.get(0);

        Assert.assertEquals(1, stmt.number);

        Assert.assertEquals("Cube", stmt.what.getName());

    }

    @Test
    public void interpreterTest2() {

        CompositeModel scene = new CompositeModel("Scene");

        List<EditorCommand> result = getInterpreter(scene).interpret("Add a cylinder or two.", scene);

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmt = (CreateEntityEditorCommand) result.get(0);

        Assert.assertTrue(1 <= stmt.number);
        Assert.assertTrue(2 >= stmt.number);

        Assert.assertEquals("Cylinder", stmt.what.getName());

    }

    @Test
    public void interpreterTestNumeric() {

        CompositeModel scene = new CompositeModel("Scene");

        List<EditorCommand> result = getInterpreter(scene).interpret("50 spheres", scene);

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmt = (CreateEntityEditorCommand) result.get(0);

        Assert.assertEquals(50, stmt.number);

        Assert.assertEquals("Sphere", stmt.what.getName());

    }

    @Test
    public void createWithAnd() {

        CompositeModel scene = new CompositeModel("Scene");

        List<EditorCommand> result = getInterpreter(scene).interpret("Add a cube and a cylinder", scene);

        Assert.assertEquals(2, result.size());


        CreateEntityEditorCommand stmt = (CreateEntityEditorCommand) result.get(0);

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        Assert.assertEquals(1, stmt.number);

        Assert.assertEquals("Cube", stmt.what.getName());


        Assert.assertTrue(result.get(1) instanceof CreateEntityEditorCommand);

        stmt = (CreateEntityEditorCommand) result.get(1);

        Assert.assertEquals(1, stmt.number);

        Assert.assertEquals("Cylinder", stmt.what.getName());

    }

    @Test
    public void createSphereAboveCube() {

        CompositeModel scene = new CompositeModel("Scene");

        List<EditorCommand> result = getInterpreter(scene).interpret("A cube above a sphere", scene);

        Assert.assertEquals(3, result.size());

        // ----------------------

        Assert.assertTrue(result.get(0) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmtA = (CreateEntityEditorCommand) result.get(0);

        Assert.assertEquals(1, stmtA.number);

        Assert.assertEquals("Cube", stmtA.what.getName());

        stmtA.apply();

        // ----------------------

        Assert.assertTrue(result.get(1) instanceof CreateEntityEditorCommand);

        CreateEntityEditorCommand stmtB = (CreateEntityEditorCommand) result.get(1);

        Assert.assertEquals(1, stmtB.number);

        Assert.assertEquals("Sphere", stmtB.what.getName());

        stmtB.apply();

        // ----------------------

        Assert.assertTrue(result.get(2) instanceof RelativePositionStatement);

        RelativePositionStatement relStmt = (RelativePositionStatement) result.get(2);

        Assert.assertEquals(stmtA.getResultSupplier().get(), relStmt.getA().get());

        Assert.assertEquals(stmtB.getResultSupplier().get(), relStmt.getB().get());

        Assert.assertEquals(ABOVE, relStmt.getPos());

    }
}