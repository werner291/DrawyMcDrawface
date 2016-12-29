package nl.wernerkroneman.Drawy.ModelEditor;

import nl.wernerkroneman.Drawy.Modelling.CompositeModel;

public interface ModelChangeListener {
    void modelChanged(CompositeModel scene);
}
