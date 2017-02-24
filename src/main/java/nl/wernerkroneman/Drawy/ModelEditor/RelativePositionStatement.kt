package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Modelling.Distance
import nl.wernerkroneman.Drawy.Modelling.RelativeConstraintContext
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint

import java.util.function.Supplier

/**
 * Represents a statement that creates a constraint
 * between two components in a CompositeModel.
 */
class RelativePositionStatement(val a: Supplier<out RelativeConstraintContext.Positionable>,
                                val b: Supplier<out RelativeConstraintContext.Positionable>,
                                val pos: RelativePositionConstraint.RelativePosition,
                                private val target: Supplier<out RelativeConstraintContext>) : EditorCommand() {

    internal override fun onApply() {
        target.get().constraints.add(RelativePositionConstraint(a.get(), b.get(), pos, Distance.ANY))
    }

    override fun toString(): String {
        return "RelativePositionStatement($a $pos $b)"
    }

}
