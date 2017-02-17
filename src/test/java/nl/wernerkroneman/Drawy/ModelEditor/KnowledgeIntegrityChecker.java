package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.*;

import java.util.Map;


public class KnowledgeIntegrityChecker implements ModelVisitor {

    private Knowledge knowledge;

    private KnowledgeIntegrityChecker(Knowledge knowledge) {

        this.knowledge = knowledge;
    }

    static boolean checkIntegrity(Knowledge knowledge) {
        KnowledgeIntegrityChecker instance = new KnowledgeIntegrityChecker(knowledge);

        for (Map.Entry<String, Model> modelEntry: knowledge.knownObjects.entrySet()) {



            //modelEntry.getValue().accept(this);
        }

        return true;
    }

    @Override
    public Object visit(GroupModel model) {
        return null;
    }

    @Override
    public Object visit(AnyModel model) {
        return null;
    }

    @Override
    public Object visit(CompositeModel model) {
        return null;
    }

    @Override
    public Object visit(PrimitiveModel model) {
        return null;
    }

    @Override
    public Object visit(PlaceholderModel model) {
        return null;
    }
}
