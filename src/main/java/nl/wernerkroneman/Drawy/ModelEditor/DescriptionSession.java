package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Interface.BlockingInteractor;
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

    private MainInterpreter interpreter;
    private BlockingInteractor interactorIface;
    private List<DescriptionSessionListener> listeners = new ArrayList<>();

    public DescriptionSession(MainInterpreter interpreter, BlockingInteractor interactorIface) {
        this.interpreter = interpreter;
        this.interactorIface = interactorIface;
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

            EditorCommand stmt = interpreter.interpret(line, scene);

            interactorIface.tellUser("Interpreted: ");

                interactorIface.tellUser(stmt.toString());

            if (interactorIface.askUserYesNo("Is this correct?")) {
                stmt.apply();
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