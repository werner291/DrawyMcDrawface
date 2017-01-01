package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.UserInteractor;

import javax.swing.*;
import java.util.concurrent.CountDownLatch;

/**
 * An interface to the UserInteractor that allows
 * calling it from another thread and blocking
 * until a response is given.
 */
public class BlockingInteractorInterface {

    UserInteractor interactor;

    public BlockingInteractorInterface(UserInteractor interactor) {
        this.interactor = interactor;
    }

    /**
     * Present a question to the user and block until a response is provided.
     * <p>
     * <em>DO NOT CALL FROM EVENT DISPATCH THREAD AS THIS MAY CAUSE A DEADLOCK!</em>
     *
     * @param question The question to ask
     * @return The response.
     */
    String askUserString(String question) {

        assert !SwingUtilities.isEventDispatchThread();

        CountDownLatch latch = new CountDownLatch(1);

        class StringWrapper {
            public String str;
        }

        final StringWrapper response = new StringWrapper();

        SwingUtilities.invokeLater(() -> {
            interactor.putText(question);

            interactor.addListener(text -> {
                response.str = text;
                latch.countDown();
                return false;
            });
        });

        while (response.str == null) {
            try {
                latch.await();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        return response.str;

    }

    boolean askUserYesNo(String question) {

        while (true) {

            String line = askUserString(question + "(Y/N)");

            if (line.equalsIgnoreCase("Y") || line.equalsIgnoreCase("Yes")) {
                return true;
            } else if (line.equalsIgnoreCase("N") || line.equalsIgnoreCase("No")) {
                return false;
            }

            System.out.println("Invalid entry: " + line);
        }

    }

    void tellUser(String toTell) {
        SwingUtilities.invokeLater(() -> {
            interactor.putText(toTell);
        });
    }
}