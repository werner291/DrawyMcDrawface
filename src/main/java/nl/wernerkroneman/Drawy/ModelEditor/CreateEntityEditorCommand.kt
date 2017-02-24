package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Modelling.CompositeModel
import nl.wernerkroneman.Drawy.Modelling.Model

import java.util.function.Supplier

/**
 * Command that causes the creation of a component
 * in a CompositeModel.
 */
class CreateEntityEditorCommand internal constructor(// A supplier that supplies the CompositeModel on which to execute this command.
        internal var target: () -> CompositeModel?) : EditorCommand() {

    // String describing what to create
    var what: Model? = null

    // After applying, stores the created component
    var created: CompositeModel.Component? = null
        internal set

    override fun toString(): String {
        return "Create $what in $target"
    }

    internal override fun onApply() {
        created = target()!!.addComponentForModel(what)
    }

    internal override fun onRevert() {
        target()!!.components.remove(created)
    }

    /**
     * Return a supplier that supplies the resulting Component.
     *
     *
     * The supplier only be executed after this function is applied,
     * throws otherwise.

     * @return the supplier.
     */
    /*val resultSupplier: Supplier<CompositeModel.Component>
        get() = object : Supplier<CompositeModel.Component> {
            override fun get(): CompositeModel.Component {
                if (!applied) {
                    throw IllegalStateException("Statement not applied")
                }
                return created
            }

            override fun toString(): String {
                return "(Created by " + this@CreateEntityEditorCommand + ")"
            }
        }*/
}
