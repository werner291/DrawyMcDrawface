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

package nl.wernerkroneman.Drawy.GlRenderer;


import com.jogamp.opengl.GL3;
import com.jogamp.opengl.GLAutoDrawable;
import com.jogamp.opengl.GLEventListener;
import nl.wernerkroneman.Drawy.ConcreteModelling.MeshFactory;
import nl.wernerkroneman.Drawy.ConcreteModelling.Scene;
import nl.wernerkroneman.Drawy.ConcreteModelling.SceneNode;

import java.util.Collections;

public class GlVisualiser implements GLEventListener {
    private static final int WINDOW_WIDTH = 640;  // width of the drawable
    private static final int WINDOW_HEIGHT = 480; // height of the drawable
    private static String TITLE = "Drawy!";  // window's title
    private final Renderer renderer = new Renderer();
    private Scene scene = new Scene(new SceneNode()); // Empty scene
    private MeshFactory meshFactory;

    /**
     * Called back by the drawable to render OpenGL graphics
     */
    @Override
    public void display(GLAutoDrawable drawable) {
        renderer.render(scene, drawable.getGL().getGL3());
    }

    /**
     * Called back immediately after the OpenGL context is initialized
     */
    @Override
    public void init(GLAutoDrawable drawable) {

        GL3 gl3 = drawable.getGL().getGL3();

        gl3.glClearColor(1, 0, 1, 1);

        renderer.initShaders(gl3);
    }

    /**
     * Called back before the OpenGL context is destroyed.
     */
    @Override
    public void dispose(GLAutoDrawable drawable) {
    }

    /**
     * Called back by the drawable when it is first set to visible,
     * and during the first repaint after the it has been resized.
     */
    @Override
    public void reshape(GLAutoDrawable drawable, int x, int y, int weight, int height) {
        renderer.computeProjection(weight, height);
    }

    public MeshFactory getMeshFactory() {
        return new GlMeshFactory();
    }

    public Scene getScene() {
        return scene;
    }

    public void setScene(Scene scene) {
        this.scene = scene;
    }
}