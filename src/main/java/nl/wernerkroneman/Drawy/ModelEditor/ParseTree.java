package nl.wernerkroneman.Drawy.ModelEditor;

import javafx.util.Pair;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree;

import java.io.StringReader;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;

class ParseTree
{
    PhraseTree rootWord;

    ParseTree(String parserOutput) {
        Stack<PhraseTree> parseStack = new Stack<>();

        Scanner strstr = new Scanner(new StringReader(parserOutput));

        while (strstr.hasNextLine())
        {
            String line = strstr.nextLine();

            // Find how far away the + is (if there is any)
            int plus = line.indexOf('+');

            // Calculate the depth based on + position
            int depth = (plus == -1) ? 0 : (plus + 4) / 4;

            if (plus > 0) {
                // Cut off up until "+--" included
                line = line.substring(plus+4);
            }

            // Split up the remainder into word/nature/role
            String[] tokens = line.split(" ");

            PhraseTree part = new PhraseTree(tokens[0], tokens[1] ,tokens[2]);

            if (depth == 0){
                // This is the root
                rootWord = part;
            }
            else
            {
                // Pop until we have the right number of ancestors
                while (depth < parseStack.size())
                {
                    parseStack.pop();
                }

                // Quick sanity check
                assert(!parseStack.empty());

                // Sentence part at the top is now parent
                parseStack.peek().addChild(part);
            }

            // Push it onto the stack so we may attach children to it if necessary
            parseStack.push(part);

        }

    }

    public String toString() {
        StringBuilder strstr = new StringBuilder();

        Stack<Pair<PhraseTree, Integer>> printStack = new Stack<>();

        printStack.push(new Pair<>(rootWord, 0));

        while (!printStack.empty())
        {
            if (strstr.length() > 0) {
                // Not the first line, prepend a newline
                strstr.append('\n');
            }

            PhraseTree word = printStack.peek().getKey();
            int depth = printStack.peek().getValue();

            printStack.pop();

            for (int i = 0; i < depth-1; i++) {
                strstr.append("    ");
            }

            if (depth > 0) {
                strstr.append(" +-- ");
            }

            strstr.append(word.getRootWord() + " " + word.getNature() + " " + word.getRole());

            List<PhraseTree> children = word.getChildren();
            for (int i = children.size()-1; i >= 0;--i) {
                PhraseTree child = children.get(i);
                printStack.push(new Pair<>(child, depth + 1));
            }
        }

        return strstr.toString();
    }

    PhraseTree getRootWord()
    {
        return rootWord;
    }
}

