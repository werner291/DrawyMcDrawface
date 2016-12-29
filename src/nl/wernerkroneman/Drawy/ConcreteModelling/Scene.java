package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Vector3d;

public class Scene {

    public static final Vector3d EYE = new Vector3d(1, 2, 5);
    public static final Vector3d LOOKAT_CENTER = new Vector3d(0);
    public static final Vector3d UP = new Vector3d(0, 1, 0);
    SceneNode rootSceneNode = new SceneNode();

    public SceneNode getRootSceneNode() {
        return rootSceneNode;
    }


}
