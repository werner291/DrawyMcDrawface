package nl.wernerkroneman.Drawy.Modelling;

import java.util.Collection;

/**
 * Marker interface that designates
 * objects that provide a valid context
 */
public interface RelativeConstraintContext {
    Collection<Constraint> getConstraints();

    // Marker interface for things that can be affected by a constraint
    interface Positionable {
    }

}
