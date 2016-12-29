package nl.wernerkroneman.Drawy.ModelEditor;/*
 * Manipulator.h
 *
 *  Created on: 10 jun. 2016
 *      Author: werner
 */


import nl.wernerkroneman.Drawy.Modelling.CompositeModel;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

class Interpreter {

    Resolver resolver;

    Interpreter(Resolver resolver) {
        this.resolver = resolver;
    }

    /**
     * Interpret a parse tree and produce a scene.
     *
     * @param toInterpret An English sentence to interpret.
     * @param previous What the scene currently looks like
     */
    List<SceneCommand> interpret(String toInterpret, CompositeModel scene) {

        List<SceneCommand> statements = new ArrayList<>();

        ParseTree tree = SyntaxNetLink.parse(toInterpret);

        assert tree != null;
        SentencePart rootWord = tree.getRootWord();

        if (!rootWord.getNature().equals("VB") || isCreationVerb(rootWord)) {
            // This is a command in the form of "Create a cube", or simply "a cube"

            SentencePart obj = rootWord.dfsFind((SentencePart part) -> part.nature.startsWith("NN"));

            if (obj == null) {
                throw new RuntimeException("Cannot find object to create.");
            }

            creationRuleForObject(statements, scene, obj);
        }

        return statements;
    }

    /**
     * Generate a creation command for the object(s) described in this sentence part.
     */
    void creationRuleForObject(List<SceneCommand> statements,
                               CompositeModel scene,
                               SentencePart obj) {

        // Allocate a new command
        CreateEntityRule createStmt = new CreateEntityRule(scene);

        // Check whether plural and singular
        String objName;

        if (obj.getNature().equals("NNS")) {
            // Plural
            createStmt.number = findNumber(obj);
            objName = pluralToSingular(obj.getRootWord());
        } else {
            // Singular
            createStmt.number = 1;
            objName = obj.getRootWord();
        }

        // Check  if this is a known object
        createStmt.what = resolver.resolveObject(objName);

        statements.add(createStmt);

        for (Iterator<SentencePart> itr = obj.children.iterator(); itr.hasNext();) {
            SentencePart part = itr.next();
            // Look for "and something else"-type phrases.
            if (part.getRole().equals("cc")) {
                if (! part.getRootWord().equals("and")) {
                    // This is an "or" or "while", don't know how to handle those yet!
                    throw new RuntimeException("I don't know how to handle coordinating conjunction "
                            + part.getRootWord() + " yet, sorry!");
                }

                // Add separate creation rules for the conjuncts as well.
                creationRuleForObject(statements, scene, itr.next());
            }
        }
    }

    /**
     * Tries to find some indication of a number of items.
     * For example, in a phrase "15 lions", this function would return 15.
     */
    int findNumber(SentencePart pPart) {
        SentencePart numeric = pPart.dfsFind((SentencePart part) -> part.getNature().equals("CD"));

        if (numeric == null) {
            throw new RuntimeException("Cannot find a number of " + pPart.getRootWord());
        }

        return interpretInteger(numeric.getRootWord());
    }

    /**
     * Returns whether this part of the sencence is a verb
     * that commands creating somehting such as "create" or "add".
     */
    boolean isCreationVerb(SentencePart part) {
        if (! part.getNature().equals("VB")) {
            return false;
        }

        String word = part.getRootWord();

        return word.equalsIgnoreCase("create") || word.equalsIgnoreCase("add");
    }

    int interpretInteger(String text) {
        return Integer.parseInt(text);
    }

    /**
     * Take a plural form of a word, and make it singular.
     * Mainly designed to work on nouns.
     *
     * For example, it converts "parties" to "party".
     */
    String pluralToSingular(String name) {
        // TODO do this properly
        if (name.endsWith("ies")) {
            return name.substring(0, name.length() - 3) + "y";
        }

        if (name.endsWith("s")) {
            return name.substring(0, name.length() - 1);
        }

        throw new RuntimeException("Cannot de-pluralize " + name);
    }

}