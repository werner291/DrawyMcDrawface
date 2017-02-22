package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.Model;
import nl.wernerkroneman.Drawy.ParseTreeMatcher.PhraseTree;

import java.util.Map;
import java.util.Stack;

/**
 * Generate a creation command for the object(s) described in this sentence part.* @param scene A supplier that provides a scene when executed in which to create the object
 */
public class KnowledgeModelInterpreter
        implements MainInterpreter.InterpretedObjectFactory<Model> {

    MainInterpreter subtreeInterpreter;
    private Knowledge knowledge;

    public KnowledgeModelInterpreter(MainInterpreter subtreeInterpreter,
                                     Knowledge knowledge) {
        this.subtreeInterpreter = subtreeInterpreter;
        this.knowledge = knowledge;
    }

    @Override
    public Class getInterpretedTypePrediction() {
        return Model.class;
    }

    @Override
    public Model createObject(Map<String, PhraseTree> capturings) {

        // Try retrieveing the object by name
        String objectName = capturings.get("object name").getRootWord();
        Model model = knowledge.getObject(objectName);

        return model;
    }
}
