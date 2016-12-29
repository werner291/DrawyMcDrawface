package nl.wernerkroneman.Drawy.ConcreteModelling;


import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.Model;
import nl.wernerkroneman.Drawy.Modelling.PrimitiveModel;
import org.joml.Vector3d;

public class AbstractToConcrete {

    MeshFactory meshFactory;
    PrimitiveGenerator primitiveGenerator;

    public AbstractToConcrete(MeshFactory meshFactory) {
        this.meshFactory = meshFactory;

        this.primitiveGenerator = new PrimitiveGenerator(meshFactory);
    }

    public static boolean hasChildWithIntersectingBB(SceneNode context, AABB proposedSolution) {
        for (SceneNode node : context.getChildren()) {
            AABB nodeBox = node.computeLocalAABB().transform(node.getTransform(), new AABB());

            if (nodeBox.intersects(proposedSolution)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Compute a conrete, drawable scene based on the abstract model.
     *
     * @param absModel An abstract model
     * @return A concrete, drawable scene
     */
    public Scene computeScene(Model absModel) {

        Scene result = new Scene();

        exploreModel(absModel, result.getRootSceneNode());

        return result;
    }

    void exploreModel(Model absModel, SceneNode node) {

        if (absModel instanceof CompositeModel) {
            /*
             * In case of a CompositeModel, every model instance
             * in the model will be given a SceneNode.
             *
             * In positioning the components, only those in the CompositeModel are taken into account.
             * Taking this higher up in the tree into account is planned later on,
             * but this should work for now.
             */
            for (CompositeModel.ModelInstance instance : ((CompositeModel) absModel).getEntities()) {
                // Create a new SceneNode for this ModelInstance
                SceneNode child = new SceneNode();

                // Generate the contents for the node
                exploreModel(instance.getBase(), child);

                // Find an empty AABB that would fit the child node.
                AABB place = findEmptyPlace(node, child.computeLocalAABB().transform(child.getTransform(), new AABB()));

                // Set the translation of the node to match the emtpy space
                Vector3d translation = getTranslationToFit(child, place);
                child.setTranslation(translation);

                // Finally, attach the child fo the parent.
                node.addChild(child);

            }
        } else if (absModel instanceof PrimitiveModel) {
            PrimitiveModel prim = (PrimitiveModel) absModel;

            node.addDrawable(new Drawable(primitiveGenerator.generateUnitCube()));
        }
    }

    Vector3d getTranslationToFit(SceneNode child, AABB place) {
        return place.minExtent.sub(child.computeLocalAABB().minExtent, new Vector3d());
    }

    AABB findEmptyPlace(SceneNode context, AABB minimumSpace) {

        double limit = 1;

        while (true) {

            Vector3d translation = new Vector3d((Math.random() - 0.5) * limit,
                    (Math.random() - 0.5) * limit,
                    (Math.random() - 0.5) * limit);

            AABB proposedSolution = minimumSpace.translate(translation, new AABB());

            if (!hasChildWithIntersectingBB(context, proposedSolution)) {
                return proposedSolution;
            } else {
                limit *= 1.1;
            }

        }

    }
}
