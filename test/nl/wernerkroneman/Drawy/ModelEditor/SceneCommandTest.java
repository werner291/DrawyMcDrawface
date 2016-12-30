package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.GroupModel;
import nl.wernerkroneman.Drawy.Modelling.PrimitiveModel;
import org.junit.Assert;
import org.junit.Test;

public class SceneCommandTest {

    @Test
    public void creationSimple() {

        CompositeModel model = new CompositeModel("Scene");

        // Verify empty scene
        Assert.assertTrue(model.getComponents().isEmpty());

        CreateEntityRule stmt = new CreateEntityRule(model);
        stmt.what = new PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder");
        stmt.number = 5;

        stmt.apply();

        Assert.assertEquals(1, model.getComponents().size());

        Assert.assertTrue(model.getComponents().get(0) instanceof GroupModel);

        GroupModel group = (GroupModel) model.getComponents().get(0);

        Assert.assertEquals(5, group.getNumber());
        Assert.assertEquals(stmt.what, group.getMember());

        stmt.revert();

        Assert.assertEquals(0, model.getComponents().size());

    }

    @Test(expected = RuntimeException.class)
    public void creationDouble() {

        CompositeModel model = new CompositeModel("Scene");

        CreateEntityRule stmt = new CreateEntityRule(model);

        stmt.apply();
        stmt.apply();

    }

}