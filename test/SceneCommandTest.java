import org.junit.Assert;
import org.junit.Test;

public class SceneCommandTest {

    @Test
    public void creationSimple() {

        CompositeModel model = new CompositeModel("Scene");

        // Verify empty scene
        Assert.assertTrue(model.entities.isEmpty());

        CreateEntityRule stmt = new CreateEntityRule(model);
        stmt.what = new PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder");
        stmt.number = 5;

        stmt.apply();

        Assert.assertEquals(1, model.entities.size());

    }

    @Test
    public void creationDouble() {

        CompositeModel model = new CompositeModel("Scene");

        // Verify empty scene
        Assert.assertTrue(model.entities.isEmpty());

        CreateEntityRule stmt = new CreateEntityRule(model);
        stmt.what = new PrimitiveModel(PrimitiveModel.ShapeType.CYLINDER, "Cylinder");
        stmt.number = 5;

        stmt.apply();

        Assert.assertEquals(1, model.entities.size());

    }

}