package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.Distance;
import nl.wernerkroneman.Drawy.Modelling.FixedDistance;

public class InterpretPhrases {

    //static Thesaurus thesaurus = new Thesaurus();

    static boolean isReciprocalPronoun(SentencePart potential) {
        return (potential.getRootWord().equalsIgnoreCase("other")
                && potential.dfsFind(part -> part.getRootWord().equalsIgnoreCase("each")) != null) ||
                (potential.getRootWord().equalsIgnoreCase("one")
                        && potential.dfsFind(part -> part.getRootWord().equalsIgnoreCase("another")) != null) ||
                potential.getRootWord().equalsIgnoreCase("eachother");
    }

    /**
     * Tries to find some indication of a number of items.
     * For example, in a phrase "15 lions", this function would return 15.
     */
    static int findNumber(SentencePart pPart) {
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
    static boolean isCreationVerb(SentencePart part) {
        if (!part.getNature().equals("VB")) {
            return false;
        }

        String word = part.getRootWord();

        return word.equalsIgnoreCase("create") || word.equalsIgnoreCase("add");
    }

    static int interpretInteger(String text) {
        return Integer.parseInt(text);
    }

    /**
     * Take a plural form of a word, and make it singular.
     * Mainly designed to work on nouns.
     * <p>
     * For example, it converts "parties" to "party".
     */
    static String pluralToSingular(String name) {
        // TODO do this properly
        if (name.endsWith("ies")) {
            return name.substring(0, name.length() - 3) + "y";
        }

        if (name.endsWith("s")) {
            return name.substring(0, name.length() - 1);
        }

        throw new RuntimeException("Cannot de-pluralize " + name);
    }

    public static Distance interpretDistance(SentencePart npadvmod) {

        SentencePart num = npadvmod.findFirstChild(c -> c.getRole().equals("num"));

        return new FixedDistance(interpretInteger(num.getRootWord()));

    }
}
