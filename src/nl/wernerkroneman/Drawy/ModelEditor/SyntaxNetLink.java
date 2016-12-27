package nl.wernerkroneman.Drawy.ModelEditor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

class SyntaxNetLink {

    static ParseTree parse(String english) {

        if (english.chars().anyMatch((int c) ->
                !(Character.isAlphabetic(c) || Character.isDigit(c) || c == '.' || c == ',' || c == ' '))) {
            throw new RuntimeException("String contains illegal or unsafe characters: " + english);
        }

        // TODO isn't it beautiful?
        try {

            ProcessBuilder processBuilder = new ProcessBuilder("docker","run","799d90a4425b","bash","-c","echo '" + english + "' | syntaxnet/demo.sh");

            Process p = processBuilder.start();

            p.waitFor();

            try (BufferedReader output=new BufferedReader(new InputStreamReader(p.getInputStream()))) {
                String line;

                int skipLines = 2;

                // Read the rest of the stream into a string
                StringBuilder str = new StringBuilder("");

                while ((line = output.readLine()) != null) {
                    if (skipLines-- <= 0) {
                        str.append(line);
                        str.append('\n');
                    }
                }

                // Parse and return
                return new ParseTree(str.toString());
            }

        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        return null;
    }

}