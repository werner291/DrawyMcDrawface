package nl.wernerkroneman.Drawy.ModelEditor

/**
 * Created by werner on 1-1-17.
 */
interface BlockingInteractorInterface {
    /**
     * Present a question to the user and block until a response is provided.
     *
     *
     * *DO NOT CALL FROM EVENT DISPATCH THREAD AS THIS MAY CAUSE A DEADLOCK!*

     * @param question The question to ask
     * *
     * @return The response.
     */
    fun askUserString(question: String): String

    fun askUserYesNo(question: String): Boolean

    fun tellUser(toTell: String)
}
