package nl.wernerkroneman.Drawy.ModelEditor;

/**
 * An action to be applied.
 *
 * It follows a Command design pattern.
 *
 * Most commands are revertible.
 */
public abstract class EditorCommand {

    // Whether this command has been applied or not.
    boolean applied = false;

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

