package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.BlockingInteractor;
import nl.wernerkroneman.Drawy.Modelling.CompositeModel;
import nl.wernerkroneman.Drawy.Modelling.Model;

import java.util.ArrayList;
import java.util.List;

/**
 * A class representing a session where a user describes an object.
 * <p>
 * It allows for asynchronous running of the session since other
 * parts of the program may need to run while this process takes its' time.
 */
public class DescriptionSession {

    private BlockingInteractor interactorIface;
    private Interpreter interpreter;
    private List<DescriptionSessionListener> listeners = new ArrayList<>();

    public DescriptionSession(Interpreter interpreter, BlockingInteractor interactorIface) {
        this.interpreter = interpreter;
        this.interactorIface = interactorIface;
    }

    public static DescriptionSession createDescriptionSession(Knowledge knowledge, BlockingInteractor iface) {

        RecursiveSessionResolver interactiveResolver = new RecursiveSessionResolver(iface);
        KnowledgeResolver knowledgeResolver = new KnowledgeResolver(knowledge, iface, interactiveResolver);
        Interpreter interpreter = new Interpreter(knowledgeResolver, knowledge, iface);
        DescriptionSession descriptionSession = new DescriptionSession(interpreter, iface);
        interactiveResolver.setSessionContext(descriptionSession);
        return descriptionSession;
    }

    public void start() {
        new Thread(this::runSession).start();
    }

    public Model runSession() {
        CompositeModel scene = new CompositeModel("Scene");
        while (true) {
            String line = interactorIface.askUserString("Say something:");

            if (line.equals("done")) {
                notifyFinished();
                break;
            }

            List<EditorCommand> results = interpreter.interpret(line, scene);

            interactorIface.tellUser("Interpreted: ");

            for (EditorCommand stmt : results) {
                interactorIface.tellUser(stmt.toString());
            }

            if (results.isEmpty()) {
                interactorIface.tellUser("Unable to interpret. Guess I'm too stupid.");
            }

            if (interactorIface.askUserYesNo("Is this correct?")) {
                for (EditorCommand cmd : results) {
                    cmd.apply();
                }
            }

            notifyChanged(scene);
        }
        return scene;
    }

    private void notifyFinished() {
        for (DescriptionSessionListener list : listeners) {
            list.sessionEnded();
        }
    }

    public void addListener(DescriptionSessionListener listener) {
        listeners.add(listener);
    }

    private void notifyChanged(CompositeModel scene) {
        for (DescriptionSessionListener list : listeners) {
            list.modelChanged(scene);
        }
    }
}