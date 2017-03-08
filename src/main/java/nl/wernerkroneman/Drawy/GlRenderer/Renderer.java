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
import nl.wernerkroneman.Drawy.ConcreteModelling.Drawable;
import nl.wernerkroneman.Drawy.ConcreteModelling.Scene;
import nl.wernerkroneman.Drawy.ConcreteModelling.SceneNode;
import org.joml.Matrix4d;
import org.joml.MatrixStackd;

import static com.jogamp.opengl.GL.*;

/**
 * Created by werner on 29-12-16.
 */
public class Renderer {

    private ShaderProgram shaderProgram;

    private MatrixStackd matrixStack = new MatrixStackd(100);
    private Matrix4d view = new Matrix4d();
    private Matrix4d proj = new Matrix4d();

    /**
     * Render the shape (triangle)
     *
     * @param scene The scene that we're trying to draw.
     * @param gl    The GL3 context provided by JOML
     */
    void render(Scene scene, GL3 gl) {

        gl.glEnable(GL_DEPTH_TEST);

        // clear the framebuffer

        gl.glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        shaderProgram.use(gl);

        // Push a matrix so the transformations in this frame do not affect those in the next
        matrixStack.pushMatrix();

        // Compute the view matrix
        view.identity().lookAt(Scene.EYE, Scene.LOOKAT_CENTER, Scene.UP);

        // Apply camera transform
        matrixStack.mul(view);

        drawSceneNode(scene.getRootSceneNode(), gl.getGL3());

        matrixStack.popMatrix();

    }

    void initShaders(GL3 gl3) {
        shaderProgram = new ShaderProgram("shaders/default_vertex.vert", "shaders/default_fragment.frag", gl3);
    }

    private void drawSceneNode(SceneNode rootSceneNode, GL3 gl) {
        matrixStack.pushMatrix();

        matrixStack.mul(rootSceneNode.getTransform()); // Apply transform

        for (SceneNode child : rootSceneNode.getChildren()) {
            drawSceneNode(child, gl); // Draw any children recursively
        }

        for (Drawable ent : rootSceneNode.getDrawables()) {
            draw(ent, gl); // Draw all entities in this node
        }

        matrixStack.popMatrix(); // Undo transforms
    }

    private void draw(Drawable ent, GL3 gl3) {

        shaderProgram.use(gl3);

        useTopMatrix(gl3);

        assert ent.getMesh() instanceof GlMesh;

        GlMesh mesh = (GlMesh) ent.getMesh();

        if (mesh.isDirty()) {
            mesh.recomputeVBO(gl3);
        }

        gl3.glBindVertexArray(mesh.getVertexArrayID());

        gl3.glDrawArrays(GL_TRIANGLES, 0, mesh.getVerticesInBuffer());

        gl3.glBindVertexArray(0);
    }

    /**
     * Send the current top matrix and projection matrix to OpenGL.
     */
    private void useTopMatrix(GL3 gl) {

        // Compute modelviewprojection as projection * modelview to get full transformation.
        Matrix4d mvp = new Matrix4d(proj).mul(matrixStack);

        // Set uniforms
        shaderProgram.setUniform("modelviewproj", mvp, gl);
        shaderProgram.setUniform("modelview", matrixStack, gl);
    }

    /**
     * Recopute the current projection matrix for a given window size.
     *
     * @param width  Window width in pixels
     * @param height Window height in pixels
     */
    public void computeProjection(int width, int height) {
        // Compute projection matrix
        proj.identity().perspective((float) Math.toRadians(45), width / (float) height, 0.1f, 1000.0f);
    }
}
