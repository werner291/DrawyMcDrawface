package nl.wernerkroneman.Drawy.ModelEditor;

/**
 * Created by werner on 1-1-17.
 */
public interface BlockingInteractorInterface {
    /**
     * Present a question to the user and block until a response is provided.
     * <p>
     * <em>DO NOT CALL FROM EVENT DISPATCH THREAD AS THIS MAY CAUSE A DEADLOCK!</em>
     *
     * @param question The question to ask
     * @return The response.
     */
    String askUserString(String question);

    boolean askUserYesNo(String question);

    void tellUser(String toTell);
}
