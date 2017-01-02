package nl.wernerkroneman.Drawy;

import nl.wernerkroneman.Drawy.ModelEditor.BlockingInteractorInterface;

import javax.swing.*;
import java.util.concurrent.CountDownLatch;

/**
 * An interface to the UserInteractor that allows
 * calling it from another thread and blocking
 * until a response is given.
 */
public class BlockingInteractor implements BlockingInteractorInterface {

    UserInteractor interactor;

    public BlockingInteractor(UserInteractor interactor) {
        this.interactor = interactor;
    }

    @Override
    public String askUserString(String question) {

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

    @Override
    public boolean askUserYesNo(String question) {

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

    @Override
    public void tellUser(String toTell) {
        SwingUtilities.invokeLater(() -> {
            interactor.putText(toTell);
        });
    }
}