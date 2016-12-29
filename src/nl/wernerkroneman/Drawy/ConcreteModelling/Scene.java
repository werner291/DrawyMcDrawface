package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Vector3d;

public class Scene {

    public static final Vector3d EYE = new Vector3d();
    public static final Vector3d LOOKAT_CENTER = new Vector3d();
    public static final Vector3d UP = new Vector3d();
    SceneNode rootSceneNode = new SceneNode();

    public SceneNode getRootSceneNode() {
        return rootSceneNode;
    }


}
