package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;

/**
 * An action to be applied on an CompositeModel.
 *
 * It follows a Command design pattern.
 *
 * Most commands are revertible.
 */
public abstract class SceneCommand {

    // Reference to the scene on which this statement is applied
    CompositeModel scene;

    // Whether this command has been applied or not.
    boolean applied = false;

    SceneCommand(CompositeModel scene) {
        this.scene = scene;
    }

    void apply() {
        if (applied) {
            throw new RuntimeException("Trying to apply a statement that was already applied!");
        }

        onApply();

        applied = true;
    }

    void revert() {
        if (!applied) {
            throw new RuntimeException("Trying to revert a non-applied statement.");
        }
        applied = false;
        onRevert();
    }

    abstract void onApply();

    void onRevert() {
        throw new UnsupportedOperationException("This command cannot be reverted.");
    }
}

