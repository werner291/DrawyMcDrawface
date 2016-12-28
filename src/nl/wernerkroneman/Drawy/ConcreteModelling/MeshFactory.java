package nl.wernerkroneman.Drawy.ConcreteModelling;

public interface MeshFactory {

    Mesh createMesh();

}

class DefaultMeshFactory implements MeshFactory {

    @Override
    public Mesh createMesh() {
        return new Mesh();
    }
}