package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.ModelEditor.CreateEntityRule;
import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.PrimitiveModel;
import org.junit.Assert;
import org.junit.Test;

public class SceneCommandTest {

    @Test
    public void creationSimple() {

        CompositeModel model = new CompositeModel("Scene");

        // Verify empty scene
        Assert.assertTrue(model.getEntities().isEmpty());

        CreateEntityRule stmt = new CreateEntityRule(model);
        stmt.what = new PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder");
        stmt.number = 5;

        stmt.apply();

        Assert.assertEquals(1, model.getEntities().size());

        stmt.revert();

        Assert.assertEquals(0, model.getEntities().size());

    }

    @Test(expected = RuntimeException.class)
    public void creationDouble() {

        CompositeModel model = new CompositeModel("Scene");

        // Verify empty scene
        Assert.assertTrue(model.getEntities().isEmpty());

        CreateEntityRule stmt = new CreateEntityRule(model);
        stmt.what = new PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder");
        stmt.number = 5;

        stmt.apply();

        Assert.assertEquals(1, model.getEntities().size());

        stmt.apply();

    }

}