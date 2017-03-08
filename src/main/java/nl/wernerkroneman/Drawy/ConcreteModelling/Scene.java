/*
 * Copyright (c) 2017 Werner Kroneman
 *
 * This file is part of DrawyMcDrawface.
 *
 * DrawyMcDrawface is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DrawyMcDrawface is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DrawyMcDrawface.  If not, see <http://www.gnu.org/licenses/>.
 */

package nl.wernerkroneman.Drawy.ConcreteModelling;

import org.joml.Vector3d;

public class Scene {

    public static final Vector3d EYE = new Vector3d(10, 20, 5);
    public static final Vector3d LOOKAT_CENTER = new Vector3d(0);
    public static final Vector3d UP = new Vector3d(0, 1, 0);
    SceneNode rootSceneNode = new SceneNode();

    public SceneNode getRootSceneNode() {
        return rootSceneNode;
    }

    public void setRootSceneNode(SceneNode rootSceneNode) {
        this.rootSceneNode = rootSceneNode;
    }


}
