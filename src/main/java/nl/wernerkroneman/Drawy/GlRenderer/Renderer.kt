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

package nl.wernerkroneman.Drawy.GlRenderer

import com.jogamp.opengl.GL3
import nl.wernerkroneman.Drawy.ConcreteModelling.Drawable
import nl.wernerkroneman.Drawy.ConcreteModelling.Material
import nl.wernerkroneman.Drawy.ConcreteModelling.Scene
import nl.wernerkroneman.Drawy.ConcreteModelling.SceneNode
import org.joml.Matrix4d
import org.joml.MatrixStackd

import com.jogamp.opengl.GL.*

/**
 * Created by werner on 29-12-16.
 */
class Renderer {

    private val matrixStack = MatrixStackd(100)
    private val view = Matrix4d()
    private val proj = Matrix4d()
    private var shaderProgram: ShaderProgram? = null

    /**
     * Render the shape (triangle)

     * @param scene The scene that we're trying to draw.
     * *
     * @param gl    The GL3 context provided by JOML
     */
    fun render(scene: Scene, gl: GL3) {

        gl.glEnable(GL_DEPTH_TEST)

        // clear the framebuffer

        gl.glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

        shaderProgram!!.use(gl)

        // Push a matrix so the transformations in this frame do not affect those in the next
        matrixStack.pushMatrix()

        // Compute the view matrix
        view.identity().lookAt(Scene.EYE, Scene.LOOKAT_CENTER, Scene.UP)

        // Apply camera transform
        matrixStack.mul(view)

        drawSceneNode(scene.rootSceneNode, gl.gL3)

        matrixStack.popMatrix()

    }

    fun initShaders(gl3: GL3) {
        shaderProgram = ShaderProgram("shaders/default_vertex.vert", "shaders/default_fragment.frag", gl3)
    }

    private fun drawSceneNode(sceneNode: SceneNode, gl: GL3) {

        matrixStack.pushMatrix()
        matrixStack.mul(sceneNode.transform) // Apply transform

        for (child in sceneNode.children) {
            drawSceneNode(child, gl) // Draw any children recursively
        }

        for (ent in sceneNode.drawables) {
            draw(ent, gl) // Draw all entities in this node
        }

        matrixStack.popMatrix() // Undo transforms
    }

    private fun draw(ent: Drawable, gl3: GL3) {

        shaderProgram!!.use(gl3)

        useTopMatrix(gl3)

        useMaterial(ent.material, gl3)

        assert(ent.mesh is GlMesh)

        val mesh = ent.mesh as GlMesh

        if (mesh.isDirty) {
            mesh.recomputeVBO(gl3)
        }

        gl3.glBindVertexArray(mesh.vertexArrayID)

        gl3.glDrawArrays(GL_TRIANGLES, 0, mesh.verticesInBuffer)

        gl3.glBindVertexArray(0)
    }

    private fun useMaterial(material: Material, gl: GL3) {

        shaderProgram!!.setUniform("mat_ambient", material.ambient, gl)
        shaderProgram!!.setUniform("mat_diffuse", material.diffuse, gl)
        shaderProgram!!.setUniform("mat_specular", material.specular, gl)
        shaderProgram!!.setUniform("mat_emissive", material.emissive, gl)

    }

    /**
     * Send the current top matrix and projection matrix to OpenGL.
     */
    private fun useTopMatrix(gl: GL3) {

        // Compute modelviewprojection as projection * modelview to get full transformation.
        val mvp = Matrix4d(proj).mul(matrixStack)

        // Set uniforms
        shaderProgram!!.setUniform("modelviewproj", mvp, gl)
        shaderProgram!!.setUniform("modelview", matrixStack, gl)
    }

    /**
     * Recopute the current projection matrix for a given window size.

     * @param width  Window lateral in pixels
     * *
     * @param height Window height in pixels
     */
    fun computeProjection(width: Int, height: Int) {
        // Compute projection matrix
        proj.identity().perspective(Math.toRadians(45.0).toFloat().toDouble(), (width / height.toFloat()).toDouble(), 0.1, 1000.0)
    }
}
