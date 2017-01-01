package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.GroupModel;
import nl.wernerkroneman.Drawy.Modelling.Model;
import nl.wernerkroneman.Drawy.Modelling.PrimitiveModel;
import org.junit.Assert;
import org.junit.Test;

public class SceneCommandTest {

    @Test
    public void creationSimple() {

        CompositeModel model = new CompositeModel("Scene");

        // Verify empty scene
        Assert.assertTrue(model.getComponents().isEmpty());

        CreateEntityCommand stmt = new CreateEntityCommand(model);
        stmt.what = new PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder");
        stmt.number = 5;

        stmt.apply();

        Assert.assertEquals(1, model.getComponents().size());

        Model model1 = model.getComponents().iterator().next().getModel();
        Assert.assertTrue(model1 instanceof GroupModel);

        GroupModel group = (GroupModel) model1;

        Assert.assertEquals(5, group.getNumber());
        Assert.assertEquals(stmt.what, group.getMemberModelType());

        stmt.revert();

        Assert.assertEquals(0, model.getComponents().size());

    }

    @Test(expected = RuntimeException.class)
    public void creationDouble() {

        CompositeModel model = new CompositeModel("Scene");

        CreateEntityCommand stmt = new CreateEntityCommand(model);

        stmt.apply();
        stmt.apply();

    }

    @Test
    public void creationWithPositionContstraint() {

        CompositeModel model = new CompositeModel("Scene");

        // Verify empty scene
        Assert.assertTrue(model.getComponents().isEmpty());

        //////////////////////

        CreateEntityCommand stmt = new CreateEntityCommand(model);
        stmt.what = new PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder");
        stmt.number = 2;
        stmt.apply();

        //////////////////////

        CreateEntityCommand stmt2 = new CreateEntityCommand(model);
        stmt2.what = new PrimitiveModel(PrimitiveModel.ShapeType.SPHERE, "Sphere");
        stmt2.number = 5;
        stmt2.apply();

        //////////////////////

        RelativePositionStatement stmt3 = new RelativePositionStatement(stmt, stmt2,
                RelativePositionStatement.RelativePosition.BELOW, model);

        stmt3.apply();

        //////////////////////

        Assert.assertEquals(2, model.getComponents().size());

        Assert.assertEquals(1, model.getConstraints().size());

    }

}