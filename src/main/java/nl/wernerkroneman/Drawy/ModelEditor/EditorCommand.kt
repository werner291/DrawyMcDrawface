package nl.wernerkroneman.Drawy.ModelEditor

/**
 * An action to be applied.

 * It follows a Command design pattern.

 * Most commands are revertible.
 */
abstract class EditorCommand {

    // Whether this command has been applied or not.
    internal var applied = false

    fun apply() {
        if (applied) {
            throw RuntimeException("Trying to apply a statement that was already applied!")
        }

        onApply()

        applied = true
    }

    fun revert() {
        if (!applied) {
            throw RuntimeException("Trying to revert a non-applied statement.")
        }
        applied = false
        onRevert()
    }

    open internal abstract fun onApply()

    open internal fun onRevert() {
        throw UnsupportedOperationException("This command cannot be reverted.")
    }
}

