package nl.wernerkroneman.Drawy.ModelEditor;

import java.util.Scanner;

/**
 * Singleton (lord forgive me) class that allows interaction with the user.
 */
public class TerminalInteractor {

    private static TerminalInteractor instance;

    static TerminalInteractor getInstance() {
        if (instance == null) {
            instance = new TerminalInteractor();
        }
        return instance;
    }

    Scanner in;

    private TerminalInteractor() {
        in = new Scanner(System.in);
    }

    String askUserString(String question) {

        System.out.println( question);
        System.out.println( ">: ");

        return in.nextLine();

    }

    boolean askUserYesNo(String question) {

        while (true) {
            System.out.println( question );
            System.out.print( "(Y/N): ");

            String line = in.nextLine();

            if (line.equalsIgnoreCase("Y")) {
                return true;
            } else if (line.equalsIgnoreCase("N")) {
                return false;
            }

            System.out.println( "Invalid entry: " + line);
        }

    }

    void tellUser(String toTell) {
        System.out.println(toTell);
    }

}