package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;

import java.util.Stack;

public class PrepositionInterpreter {

    final PhraseMatcherMap<StandardPhraseMatchers.RelativePositionParams> positionPhrases;
    private Interpreter interpreter;

    public PrepositionInterpreter(PhraseMatcherMap<StandardPhraseMatchers.RelativePositionParams> relativePositionParamsPhraseMatcherMap) {
        this.positionPhrases = relativePositionParamsPhraseMatcherMap;
    }

    public void setInterpreter(Interpreter interpreter) {
        this.interpreter = interpreter;
    }

    /**
     * Explore a preposition.
     * <p>
     * Ex: "above the field", "in the room", "through the tunnel"
     *
     * @param preposition The preposition
     * @param relatesTo   A stack of models that the preposition might affect.
     *                    <p>
     *                    {@code relatesTo} is a stack of models on which the preposition might be applied.
     *                    <p>
     *                    For example, the phrase "a car on a road" will be broken down into "a car" and "on a road".
     *                    "a car" is evaluated first, then processPreposition is called with a stack with the {@link Model}
     *                    representing the car at the top.
     *                    <p>
     *                    processPreposition would then pop the car Model off the stack, add a {@link CompositeModel} onto it,
     *                    create a road Model, add it as a component to the CompositeModel, add the car as a component to the
     *                    CompositeModel, and add a constraint to the CompositeModel that the car is on the road.
     */
    void processPreposition(SentencePart preposition,
                            Stack<Model> relatesTo) {

        // Allocate the constraint
        RelativePositionConstraint positionConstraint = new RelativePositionConstraint();

        // Match the preposition against one of the known preposition patterns.

        PhraseMatcherMap<StandardPhraseMatchers.RelativePositionParams>.EntryMatchResult match = positionPhrases.findMatch(preposition);

        if (!match.matchResult.matches) {
            throw new UnsupportedOperationException("I don't know the preposition " + preposition.getRootWord());
        }

        positionConstraint.pos = match.entry.value.pos;

        /////////////////////////////////
        // Find the preposition object //
        /////////////////////////////////

        // Find what the preposition relates to.
        // For example, in "on a sphere", this would be "a sphere"
        SentencePart prepObj = match.matchResult.capturings.get("relObj");

        // This is assumed to be impossible, but better check to make sure.
        if (prepObj == null) {
            throw new IllegalStateException("Preposition without object.");
        }

        // Find the determinant of the pobj
        SentencePart pobjDet = prepObj.findFirstChild(part -> part.getRole().equals("det"));

        /////////////////////////////
        // Find adverbial modifier //
        /////////////////////////////

        SentencePart npadvmod = preposition.findFirstChild(c -> c.getRole().equals("npadvmod"));

        if (npadvmod != null) {
            positionConstraint.dist = InterpretPhrases.interpretDistance(npadvmod);
        }

        if (InterpretPhrases.isReciprocalPronoun(prepObj)) {
            // Reciprocal pronoun ("each other", "one another")

            if (relatesTo.peek() instanceof GroupModel) {
                // Reciprocal pronoun prepositions apply between each element and the next,
                // use the placeholders to indicate this.
                positionConstraint.a = GroupModel.PLACEHOLDER_A;
                positionConstraint.b = GroupModel.PLACEHOLDER_B;
            } else {
                throw new UnsupportedOperationException("Reciprocal pronoun as object of proposition on " +
                        relatesTo.peek().getClass().getName() + " not supported or implemented.");
            }

        } else if (pobjDet == null ||
                  (pobjDet != null &&
                          pobjDet.getRootWord().equalsIgnoreCase("a"))) {

            // Preposition calls for a relation with an object that still needs to be created
            // Swap the stack top for a CompositeModel that has the stack top as a component.
            CompositeModel compositeModel = new CompositeModel(preposition.getRootWord());
            Model topModel = relatesTo.pop();
            relatesTo.push(compositeModel);

            // Set the constraints related objects as the topModel and the new object.
            positionConstraint.a = compositeModel.addComponentForModel(topModel);
            positionConstraint.b = compositeModel.addComponentForModel(
                    interpreter.interpretModel(prepObj, relatesTo));

        } else {

            throw new UnsupportedOperationException("Preposition with object like " + prepObj
                    + " not supported or implemented yet.");
        }

        if (!(relatesTo.peek() instanceof RelativeConstraintContext)) {
            // This should not occur, but check it just to make sure.
            throw new IllegalStateException("Trying to apply relative constraint on something that is not a relative " +
                    "constraint context.");
        }

        // Apply the constraint.
        ((RelativeConstraintContext) relatesTo.peek()).getConstraints().add(positionConstraint);
    }
}