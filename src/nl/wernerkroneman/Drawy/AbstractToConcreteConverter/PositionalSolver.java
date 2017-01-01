package nl.wernerkroneman.Drawy.AbstractToConcreteConverter;

import nl.wernerkroneman.Drawy.ConcreteModelling.AABB;
import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.Constraint;
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint;
import org.joml.Vector3d;

import java.util.Collection;
import java.util.List;
import java.util.Set;

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
     * TODO: this one is fishy, need to re-assess this.
     *
     * @param components  The list of components
     * @param constraints The constraints on those components.
     */
    static void sortIntoFeasibleOrder(List<CompositeModel.Component> components, Set<Constraint> constraints) {
        components.sort((a, b) -> {
            // Does this work with a partial ordering?
            int result = 0;

            for (Constraint constr : constraints) {
                if (constr instanceof RelativePositionConstraint) {
                    RelativePositionConstraint constrPos = (RelativePositionConstraint) constr;

                    int recommended = requiresOrder(a, b, constrPos);

                    if (result != 0 && recommended != result) {
                        throw new IllegalStateException("Constraints unsolvable.");
                    }

                    result = recommended;
                }
            }

            return result;
        });
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
