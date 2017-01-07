package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.Distance;
import nl.wernerkroneman.Drawy.Modelling.FixedDistance;
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint;

import static nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.*;

public class StandardPhraseMatchers {

    static class RelativePositionParams {
        public RelativePositionParams(RelativePositionConstraint.RelativePosition pos, Distance dist) {
            this.pos = pos;
            this.dist = dist;
        }

        RelativePositionConstraint.RelativePosition pos;
        Distance dist;
    }

    static PhraseMatcherMap<RelativePositionParams> relativePositionPhrases() {

        PhraseMatcherMap<RelativePositionParams> phrases = new PhraseMatcherMap<RelativePositionParams>();

        phrases.addPattern(PhraseMatcher.compilePattern("in * *\n" +
                                                        " front * *\n" +
                                                        "  of * *\n" +
                                                        "   * * pobj relObj"),
                new RelativePositionParams(FRONT, Distance.ANY));

        phrases.addPattern(PhraseMatcher.compilePattern("behind * *\n" +
                                                        " * * pobj relObj"),
                new RelativePositionParams(BEHIND, Distance.ANY));

        phrases.addPattern(PhraseMatcher.compilePattern("above * *\n" +
                                                        " * * pobj relObj"),
                new RelativePositionParams(ABOVE, Distance.ANY));

        phrases.addPattern(PhraseMatcher.compilePattern("on * *\n" +
                                                        " top * *\n" +
                                                        "  of * *\n" +
                                                        "   * * pobj relObj"),
                new RelativePositionParams(ABOVE, new FixedDistance(0)));

        return phrases;
    }

}
