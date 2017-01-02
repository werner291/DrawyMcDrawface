package nl.wernerkroneman.Drawy.AbstractToConcreteConverter;


import nl.wernerkroneman.Drawy.ConcreteModelling.*;
import nl.wernerkroneman.Drawy.Modelling.*;
import org.joml.Vector3d;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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
        resolveThroughPositionResolutionContext(node, new PositionResolutionContext(absModel));
    }

    private void resolveThroughPositionResolutionContext(SceneNode node, PositionResolutionContext
            positionResolutionContext) {
        // Translate the set of components into a list such that positional constraints
        // can be solved in the right order.

        // List of AABBs that count as "occupied" space.
        List<AABB> occupiedSpaces = new ArrayList<>();

        // Iterate voer all components.
        for (PositionResolutionContext.Component component : positionResolutionContext.getComponents()) {

            // Create a new SceneNode for this ModelInstance
            component.node = new SceneNode();

            // Attach the child to the parent.
            node.addChild(component.node);

            // Generate the contents for the node
            createSceneNodeForModel(component.abstractModel, component.node);

            placeChild(component, occupiedSpaces);

            // Mark the space as occupied.
            occupiedSpaces.add(component.node.computeWorldAABB());

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
        resolveThroughPositionResolutionContext(node, new PositionResolutionContext(absModel));
    }

    /**
     * Place (translate) the child in the composite model scene node.
     *
     * @param component      The component to place
     * @param occupiedSpaces A list of AABBs that are considered "occupied"
     */
    void placeChild(PositionResolutionContext.Component component,
                    Collection<AABB> occupiedSpaces) {

        AABB allowedSpace = computeAllowedSpace(component);

        // Compute how big the AABB of the child is.
        AABB childRequiredAABB = component.node.computeLocalAABB();

        // Find an empty AABB inside that space.
        AABB place = PositionalSolver.findEmptyPlace(occupiedSpaces, 0, childRequiredAABB, allowedSpace);

        // Set the translation of the node to match the empty space
        Vector3d translation = getTranslationToFit(component.node, place);

        // Translate the child
        component.node.setTranslation(translation);
    }

    /**
     * Compute an AABB in which the new object can be placed
     * in accordance to the provided constraints.
     *
     * @param toPlace Which component we're trying to place.
     */
    private AABB computeAllowedSpace(PositionResolutionContext.Component toPlace) {

        AABB restrictSpace = new AABB(new Vector3d(Double.POSITIVE_INFINITY), new Vector3d(Double.NEGATIVE_INFINITY));

        for (PositionResolutionContext.Component.Constraint constr : toPlace.constraintRelatedTo) {
            if (constr.abstractConstraint instanceof RelativePositionConstraint) {

                RelativePositionConstraint posConstrl = constr.abstractConstraint;

                assert constr.relativeTo.node != null;

                // Compute the AABB of the object that toPlace is related to.
                AABB relatedBBounds = constr.relativeTo.node.computeLocalAABB().transform(constr.relativeTo.node
                        .getTransform(), new AABB());

                // Compute the infinite AABB above the related object
                AABB aboveSpace = new AABB(relatedBBounds);

                aboveSpace.maxExtent.y = Double.POSITIVE_INFINITY;
                aboveSpace.minExtent.y = relatedBBounds.maxExtent.y;

                restrictSpace = restrictSpace.intersection(aboveSpace, restrictSpace);
            }
        }

        return restrictSpace;
    }

    public Vector3d getTranslationToFit(SceneNode child, AABB place) {
        return place.minExtent.sub(child.computeLocalAABB().minExtent, new Vector3d());
    }

}
