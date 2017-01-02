package nl.wernerkroneman.Drawy.AbstractToConcreteConverter;

import nl.wernerkroneman.Drawy.ConcreteModelling.AABB;
import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.Constraint;
import nl.wernerkroneman.Drawy.Modelling.RelativeConstraintContext;
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint;
import org.joml.Vector3d;

import java.util.*;

public class PositionalSolver {

    /**
     * Find an axis-aligned box that at is at least big enough to contain {@code minimumSpace}
     * (same orientation) whose center is roughly central in {@code centerInsideSpace}
     * and which must not intersect any in {@code mustNotIntersect}
     *
     * @param mustNotIntersect  Collection of AABBs that must not be intersected
     * @param minimumSpace      The minimum size space to find
     * @param centerInsideSpace In which space to center the AABB
     * @return The AABB, or null
     */
    public static AABB findEmptyPlace(Collection<AABB> mustNotIntersect, double tolerance, AABB minimumSpace, AABB
            centerInsideSpace) {

        double limit = 0;

        AABB generateSpace = new AABB();

        while (true) {

            if (centerInsideSpace == null) {
                generateSpace.minExtent.set(-limit / 2, -limit / 2, -limit / 2);
                generateSpace.maxExtent.set(limit / 2, limit / 2, limit / 2);
            } else {
                centerInsideSpace.getFiniteWithBounds(limit, limit, limit, generateSpace);
            }

            Vector3d translation = new Vector3d(
                    generateSpace.minExtent.x + Math.random() * generateSpace.getWidth(),
                    generateSpace.minExtent.y + Math.random() * generateSpace.getHeight(),
                    generateSpace.minExtent.z + Math.random() * generateSpace.getDepth()
            );

            AABB proposedSolution = minimumSpace.translate(translation, new AABB());

            boolean intersectsAny = false;

            for (AABB aabb : mustNotIntersect) {
                if (aabb.intersects(proposedSolution, tolerance)) {
                    intersectsAny = true;
                    break;
                }
            }

            if (intersectsAny) {
                limit += 0.1;
            } else {
                return proposedSolution;
            }

        }

    }

    /**
     * Sort the provided list of components (in place) such that
     * the components can be placed without running into problems
     * with positions that depend on eachother.
     * <p>
     *
     * @param components  The list of components
     * @param constraints The constraints on those components.
     */
    static List<? extends RelativeConstraintContext.Positionable> sortIntoFeasibleOrder(Set<? extends
            RelativeConstraintContext.Positionable> components,
                                                                                        Collection<Constraint>
                                                                                                constraints) {

        Map<RelativeConstraintContext.Positionable, List<RelativeConstraintContext.Positionable>> componentGraphOut =
                new HashMap<>();
        Map<RelativeConstraintContext.Positionable, List<RelativeConstraintContext.Positionable>> componentGraphIn = new HashMap<>();

        for (RelativeConstraintContext.Positionable comp : components) {
            componentGraphIn.put(comp, new ArrayList<>());
            componentGraphOut.put(comp, new ArrayList<>());
        }

        for (Constraint constr : constraints) {
            if (constr instanceof RelativePositionConstraint) {
                RelativePositionConstraint posConstr = (RelativePositionConstraint) constr;

                componentGraphOut.get(posConstr.getB()).add(posConstr.getA());
                componentGraphIn.get(posConstr.getA()).add(posConstr.getB());
            }
        }

        Queue<RelativeConstraintContext.Positionable> noIncoming = new LinkedList<>();

        for (RelativeConstraintContext.Positionable comp : components) {
            if (componentGraphIn.get(comp).isEmpty()) {
                noIncoming.add(comp);
            }
        }

        List<RelativeConstraintContext.Positionable> result = new ArrayList<>();

        while (!noIncoming.isEmpty()) {
            RelativeConstraintContext.Positionable comp = noIncoming.poll();

            result.add(comp);

            for (RelativeConstraintContext.Positionable dependant : componentGraphOut.get(comp)) {
                componentGraphIn.get(dependant).remove(comp);
                if (componentGraphIn.get(dependant).isEmpty()) {
                    noIncoming.add(dependant);
                }
            }

            componentGraphOut.remove(comp);
        }

        if (result.size() < components.size()) {
            throw new IllegalStateException("Constraints not solvable.");
        }

        return result;
    }

    /**
     * @param a
     * @param b
     * @param constrPos
     * @return
     */
    private static int requiresOrder(CompositeModel.Component a, CompositeModel.Component b,
                                     RelativePositionConstraint constrPos) {
        if (constrPos.getA() == a && constrPos.getB() == b) {
            return 1;
        } else if (constrPos.getA() == b && constrPos.getB() == a) {
            return -1;
        }
        return 0;
    }
}
