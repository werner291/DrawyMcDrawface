package nl.wernerkroneman.Drawy.GlRenderer;

import nl.wernerkroneman.Drawy.ConcreteModelling.Mesh;
import nl.wernerkroneman.Drawy.ConcreteModelling.MeshFactory;


public class GlMeshFactory implements MeshFactory {
    @Override
    public Mesh createMesh() {
        return new GlMesh();
    }
}

