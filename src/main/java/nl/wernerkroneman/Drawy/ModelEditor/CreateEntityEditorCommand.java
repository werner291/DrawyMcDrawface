package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.Model;

import java.util.function.Supplier;

/**
 * Command that causes the creation of a component
 * in a CompositeModel.
 */
public class CreateEntityEditorCommand extends EditorCommand {

    // String describing what to create
    Model what;

    // A supplier that supplies the CompositeModel on which to execute this command.
    Supplier<CompositeModel> target;

    // After applying, stores the created component
    CompositeModel.Component created;

    CreateEntityEditorCommand(Supplier<CompositeModel> target) {
        super();
        this.target = target;
    }

    @Override
    public String toString() {
        return "Create " + what + " in " + target;
    }

    @Override
    void onApply(){
        created = target.get().addComponentForModel(what);
    }

    @Override
    void onRevert() {
        target.get().getComponents().remove(created);
    }

    public CompositeModel.Component getCreated() {
        return created;
    }

    /**
     * Return a supplier that supplies the resulting Component.
     * <p>
     * The supplier only be executed after this function is applied,
     * throws otherwise.
     *
     * @return the supplier.
     */
    public Supplier<CompositeModel.Component> getResultSupplier() {
        return new Supplier<CompositeModel.Component>() {
            @Override
            public CompositeModel.Component get() {
                if (!applied) {
                    throw new IllegalStateException("Statement not applied");
                }
                return created;
            }

            @Override
            public String toString() {
                return "(Created by " + (CreateEntityEditorCommand.this) + ")";
            }
        };
    }
}
