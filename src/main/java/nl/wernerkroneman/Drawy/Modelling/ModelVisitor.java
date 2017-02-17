package nl.wernerkroneman.Drawy.Modelling;

public interface ModelVisitor<ReturnT extends Object> {

    ReturnT visit(GroupModel model);

    ReturnT visit(AnyModel model);

    ReturnT visit(CompositeModel model);

    ReturnT visit(PrimitiveModel model);

    ReturnT visit(PlaceholderModel model);
}
