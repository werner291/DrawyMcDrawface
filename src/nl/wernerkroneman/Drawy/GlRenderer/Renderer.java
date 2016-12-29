package nl.wernerkroneman.Drawy.GlRenderer;

import com.jogamp.opengl.GL3;
import com.jogamp.opengl.util.glsl.ShaderCode;
import com.jogamp.opengl.util.glsl.ShaderProgram;
import nl.wernerkroneman.Drawy.ConcreteModelling.Drawable;
import nl.wernerkroneman.Drawy.ConcreteModelling.Scene;
import nl.wernerkroneman.Drawy.ConcreteModelling.SceneNode;
import org.joml.Matrix4d;
import org.joml.MatrixStackd;

import static com.jogamp.opengl.GL.*;
import static com.jogamp.opengl.GL2ES2.GL_FRAGMENT_SHADER;
import static com.jogamp.opengl.GL2ES2.GL_VERTEX_SHADER;

/**
 * Created by werner on 29-12-16.
 */
public class Renderer {
    static final String SHADERS_ROOT = "shaders";  // Renderer
    private int shaderProgram;

    private MatrixStackd matrixStack = new MatrixStackd(100);
    private Matrix4d view = new Matrix4d();

    /**
     * Render the shape (triangle)
     *
     * @param scene The scene that we're trying to draw.
     * @param gl    The GL3 context provided by JOML
     */
    void render(Scene scene, GL3 gl) {

        // clear the framebuffer
        gl.glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        gl.glUseProgram(shaderProgram);

        // Push a matrix so the transformations in this frame do not affect those in the next
        matrixStack.pushMatrix();

        // Compute the view matrix
        view.identity().lookAt(Scene.EYE, Scene.LOOKAT_CENTER, Scene.UP);

        // Apply camera transform
        matrixStack.mul(view);

        // Scale the y by -1 to get a left-handed coordinate system
        // (would it be better to stick to right-handed, and put the translation in TransitionState?)
        matrixStack.scale(1, -1, 1);

        drawSceneNode(scene.getRootSceneNode(), gl.getGL3());

        matrixStack.popMatrix();

    }

    void initShaders(GL3 gl3) {
        ShaderCode fragShader = ShaderCode.create(gl3, GL_FRAGMENT_SHADER, this.getClass(), SHADERS_ROOT, null,
                "default_fragment", "frag", null, false);
        ShaderCode vertShader = ShaderCode.create(gl3, GL_VERTEX_SHADER, this.getClass(), SHADERS_ROOT, null,
                "default_vertex", "vert", null, false);

        ShaderProgram prog = new ShaderProgram();
        //prog.add(vertShader);
        prog.add(fragShader);

        prog.init(gl3);

        shaderProgram = prog.program();

        gl3.glBindAttribLocation(shaderProgram, ShaderPositions.POSITION, "position");
        gl3.glBindAttribLocation(shaderProgram, ShaderPositions.COLOR, "color");

        prog.link(gl3, System.err);

        vertShader.destroy(gl3);
        fragShader.destroy(gl3);
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

        gl3.glUseProgram(shaderProgram);

        assert ent.getMesh() instanceof GlMesh;

        GlMesh mesh = (GlMesh) ent.getMesh();

        gl3.glBindVertexArray(mesh.getVertexArrayID());

        gl3.glDrawArrays(GL_TRIANGLES, 1, mesh.getVerticesInBuffer());

        gl3.glBindVertexArray(0);
    }
}
