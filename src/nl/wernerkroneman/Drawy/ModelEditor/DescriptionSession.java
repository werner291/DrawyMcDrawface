package nl.wernerkroneman.Drawy.ModelEditor;//
// Created by werner on 25-12-16.


import nl.wernerkroneman.Drawy.Modelling.CompositeModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class DescriptionSession {

    private List<ModelChangeListener> listeners = new ArrayList<>();

    public CompositeModel descriptionSession(String subject, Knowledge knowledge) {

        CompositeModel scene = new CompositeModel("subject");

        System.out.print(subject + ">: ");

        Scanner in = new Scanner(System.in);

        Resolver resolver = new Resolver(TerminalInteractor.getInstance(), knowledge);

        Interpreter interpreter = new Interpreter(resolver);

        while (in.hasNextLine()) {

            String line = in.nextLine();

            if (line.equals("done")) {
                break;
            }

            List<SceneCommand> results = interpreter.interpret(line, scene);

            System.out.println("Interpreted: ");

            for (SceneCommand stmt : results) {
                System.out.println(stmt.toString());
            }

            if (results.isEmpty()) {
                System.out.println("Unable to interpret. Guess I'm too stupid.");
            }

            if (TerminalInteractor.getInstance().askUserYesNo("Is this correct?")) {
                for (SceneCommand cmd : results) {
                    cmd.apply();
                }
            }

            notifyChanged(scene);

            System.out.println(subject);
        }

        return scene;
    }

    public void addListener(ModelChangeListener listener) {
        listeners.add(listener);
    }

    private void notifyChanged(CompositeModel scene) {
        for (ModelChangeListener list : listeners) {
            list.modelChanged(scene);
        }
    }

}