/*
 * Copyright (c) 2017 Werner Kroneman
 *
 * This file is part of DrawyMcDrawface.
 *
 * DrawyMcDrawface is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DrawyMcDrawface is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DrawyMcDrawface.  If not, see <http://www.gnu.org/licenses/>.
 */

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
     * Find an axis-aligned box that at is at least big enough to contain an
     * axis-aligned box with size {@code minimumSpaceSize} (same orientation)
     * whose center is roughly central in {@code fitInsideSpace} and which
     * must not intersect any in {@code mustNotIntersect}
     *
     * @param mustNotIntersect  Collection of AABBs that must not be intersected
     * @param minimumSpaceSize The minimum size space to find
     * @param fitInsideSpace In which space to center the AABB
     * @return The AABB
     */
    public static AABB findEmptyPlace(Collection<AABB> mustNotIntersect,
                                      double tolerance,
                                      Vector3d minimumSpaceSize,
                                      AABB fitInsideSpace) {

        double limit = 0;

        AABB initialSolution = fitInsideSpace.getFiniteWithBounds(minimumSpaceSize.x,
                minimumSpaceSize.y,
                minimumSpaceSize.z, new AABB());

        final AABB candidate = new AABB(initialSolution);

        int triesLeft = 10000;

        while (mustNotIntersect.stream().anyMatch(aabb -> aabb.intersects(candidate, tolerance)) && triesLeft-- > 0) {

            // Find out what wiggle room we have
            double translateMinX = Math.max(fitInsideSpace.minExtent.x - initialSolution.minExtent.x, - limit);
            double translateMinY = Math.max(fitInsideSpace.minExtent.y - initialSolution.minExtent.y, - limit);
            double translateMinZ = Math.max(fitInsideSpace.minExtent.z - initialSolution.minExtent.z, - limit);

            double translateMaxX = Math.min(fitInsideSpace.maxExtent.x - initialSolution.maxExtent.x, limit);
            double translateMaxY = Math.min(fitInsideSpace.maxExtent.y - initialSolution.maxExtent.y, limit);
            double translateMaxZ = Math.min(fitInsideSpace.maxExtent.z - initialSolution.maxExtent.z, limit);

            Vector3d translation = new Vector3d(
                    translateMinX + Math.random() * (translateMaxX - translateMinX),
                    translateMinY + Math.random() * (translateMaxY - translateMinY),
                    translateMinZ + Math.random() * (translateMaxZ - translateMinZ)
            );

            initialSolution.translate(translation, candidate);

            limit += 0.1;

        }

        if (triesLeft <= 0) {
            return null; // Give up.
        }

        return candidate;

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
