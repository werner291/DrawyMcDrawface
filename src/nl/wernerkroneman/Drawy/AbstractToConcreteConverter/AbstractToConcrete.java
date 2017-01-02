package nl.wernerkroneman.Drawy.AbstractToConcreteConverter;


import nl.wernerkroneman.Drawy.ConcreteModelling.*;
import nl.wernerkroneman.Drawy.Modelling.*;
import org.joml.Vector3d;

import java.util.*;

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

            if (nodeBox.intersects(proposedSolution, 0)) {
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

        createSceneNodeForModel(absModel, result.getRootSceneNode());

        return result;
    }

    /**
     * Compute the contents of the scene node in accordance to the model.
     *
     * @param absModel The model to base the scene node on.
     * @param node     The scene node that represents the model.
     */
    void createSceneNodeForModel(Model absModel, SceneNode node) {
        if (absModel instanceof CompositeModel) {
            exploreCompositeModel((CompositeModel) absModel, node);
        } else if (absModel instanceof GroupModel) {
            computeSceneNodeForGroupModel((GroupModel) absModel, node);
        } else if (absModel instanceof PrimitiveModel) {
            computeSceneNodeForPrimitive((PrimitiveModel) absModel, node);
        }
    }

    private void computeSceneNodeForPrimitive(PrimitiveModel absModel, SceneNode node) {
        PrimitiveModel prim = absModel;

        switch (prim.getShape()) {
            case CUBE:
                node.addDrawable(new Drawable(primitiveGenerator.generateUnitCube()));
                break;
            case SPHERE:
                node.addDrawable(new Drawable(primitiveGenerator.generateSphere(0.5, 16, 8)));
                break;
            default:
                throw new UnsupportedOperationException("Shape " + prim.getShape() + " not implemented.");
        }
    }

    /**
     * In case of a CompositeModel, every model instance
     * in the model will be given a SceneNode.
     * <p>
     * In positioning the components, only those in the CompositeModel are taken into account.
     * Taking this higher up in the tree into account is planned later on,
     * but this should work for now.
     */
    private void computeSceneNodeForGroupModel(GroupModel absModel, SceneNode node) {

        GroupModel group = absModel;

        // Maintain a list of occupied AABBs
        List<AABB> occupiedSpaces = new ArrayList<>();

        for (int i = 0; i < group.getNumber(); i++) {
            // Create a new SceneNode for this ModelInstance
            SceneNode child = new SceneNode();

            // Generate the contents for the node
            createSceneNodeForModel(group.getMemberModelType(), child);

            // Compute the space required for the child.
            AABB childRequiredAABB = child.computeLocalAABB().transform(child.getTransform(), new AABB());

            // Find an empty place for the child
            AABB place = PositionalSolver.findEmptyPlace(occupiedSpaces, 0.001, childRequiredAABB, null);

            // Set the translation of the node to match the empty space
            Vector3d translation = getTranslationToFit(child, place);

            // Translate the child
            child.setTranslation(translation);

            // Mark the space as occupied.
            occupiedSpaces.add(place);

            // Finally, attach the child to the parent.
            node.addChild(child);

        }
    }

    /**
     * In case of a CompositeModel, every model instance
     * in the model will be given a SceneNode.
     * <p>
     * In positioning the components, only those in the CompositeModel are taken into account.
     * Taking this higher up in the tree into account is planned later on,
     * but this should work for now.
     */
    private void exploreCompositeModel(CompositeModel absModel, SceneNode node) {

        // Translate the set of components into a list such that positional constraints
        // can be solved in the right order.
        List<CompositeModel.Component> components = (List<CompositeModel.Component>) PositionalSolver
                .sortIntoFeasibleOrder(absModel.getComponents(),
                absModel.getConstraints());

        // Maintain a mapping of components to the scene nodes that were produced from them.
        Map<CompositeModel.Component, SceneNode> componentToNode = new HashMap<>();

        // List of AABBs that count as "occupied" space.
        List<AABB> occupiedSpaces = new ArrayList<>();

        // Iterate voer all components.
        for (CompositeModel.Component instance : components) {

            // Create a new SceneNode for this ModelInstance
            SceneNode child = new SceneNode();

            componentToNode.put(instance, child);

            // Generate the contents for the node
            createSceneNodeForModel(instance.getModel(), child);

            AABB allowedSpace = computeAllowedSpace(componentToNode, absModel.getConstraints(), instance);

            // Compute how big the AABB of the child is.
            AABB childRequiredAABB = child.computeLocalAABB().transform(child.getTransform(), new AABB());

            // Find an empty AABB inside that space.
            AABB place = PositionalSolver.findEmptyPlace(occupiedSpaces, 0, childRequiredAABB, allowedSpace);

            // Set the translation of the node to match the empty space
            Vector3d translation = getTranslationToFit(child, place);

            // Translate the child
            child.setTranslation(translation);

            // Mark the space as occupied.
            occupiedSpaces.add(place);

            // Finally, attach the child to the parent.
            node.addChild(child);

        }
    }

    /**
     * Compute an AABB in which the new object can be placed
     * in accordance to the provided constraints.
     *
     * @param componentToNode A map of components to scenenodes, the previously-placed nodes.
     * @param constraints     A set of constraints to satisfy
     * @param toPlace         Which component we're trying to place.
     */
    private AABB computeAllowedSpace(Map<CompositeModel.Component, SceneNode> componentToNode,
                                     Collection<Constraint> constraints,
                                     CompositeModel.Component toPlace) {

        AABB restrictSpace = new AABB(new Vector3d(Double.POSITIVE_INFINITY), new Vector3d(Double.NEGATIVE_INFINITY));

        for (Constraint constr : constraints) {
            if (constr instanceof RelativePositionConstraint) {

                RelativePositionConstraint posConstrl = (RelativePositionConstraint) constr;

                // This constraint must be about the position of "toPlace"
                // relative to some other shape
                if (posConstrl.getA() == toPlace) {

                    SceneNode relatedB = componentToNode.get(posConstrl.getB());

                    assert (relatedB != null);

                    // Compute the AABB of the object that toPlace is related to.
                    AABB relatedBBounds = relatedB.computeLocalAABB().transform(relatedB.getTransform(), new AABB());

                    // Compute the infinite AABB above the related object
                    AABB aboveSpace = new AABB(relatedBBounds);

                    aboveSpace.maxExtent.y = Double.POSITIVE_INFINITY;
                    aboveSpace.minExtent.y = relatedBBounds.maxExtent.y;

                    restrictSpace = restrictSpace.intersection(aboveSpace, restrictSpace);
                }
            }
        }

        return restrictSpace;
    }

    public Vector3d getTranslationToFit(SceneNode child, AABB place) {
        return place.minExtent.sub(child.computeLocalAABB().minExtent, new Vector3d());
    }

}
