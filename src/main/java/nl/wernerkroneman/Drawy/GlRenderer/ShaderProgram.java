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
import org.joml.Matrix4d;
import org.joml.Vector3f;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;

import static com.jogamp.opengl.GL2ES2.*;

/**
 * Class representing a fully-compiled GLSL shader program.
 */
class ShaderProgram {

    private int glShaderProgram;

    /**
     * Create a program from two files.
     *
     * @param vertexShaderPath   Path to the vertex shader
     * @param fragmentShaderPath Path to the fragment shader
     */
    public ShaderProgram(String vertexShaderPath, String fragmentShaderPath, GL3 gl3) {

        // Compile the individual shaders.
        int vertexShaderID = loadAndCompileShader(vertexShaderPath, GL_VERTEX_SHADER, gl3);
        int fragmentShaderID = loadAndCompileShader(fragmentShaderPath, GL_FRAGMENT_SHADER, gl3);

        // Put them toghether in a progra,
        int ProgramID = gl3.glCreateProgram();
        gl3.glAttachShader(ProgramID, vertexShaderID);
        gl3.glAttachShader(ProgramID, fragmentShaderID);

        gl3.glBindAttribLocation(ProgramID, ShaderPositions.POSITION, "position");

        gl3.glLinkProgram(ProgramID);

        int[] status = new int[1];
        gl3.glGetProgramiv(ProgramID, GL_LINK_STATUS, status, 0);

        // Check for link errors.
        if (status[0] == GL_FALSE) {

            int[] length = new int[1];
            byte[] buffer = new byte[1000];

            gl3.glGetProgramInfoLog(ProgramID, 1000, length, 0, buffer, 0);

            final String errorString = "linking error for shader. Reason: " + Arrays.toString(buffer);
            throw new IllegalStateException(errorString);
        }

        this.glShaderProgram = ProgramID;
    }

    /**
     * Load a shader from the specified filepath.
     *
     * @param filepath Path to the file.
     * @param type     The type of the shader  (such as GL_VERTEX_SHADER)
     * @return The OpenGL id of the shader.
     */
    private static int loadAndCompileShader(String filepath, int type, GL3 gl3) {
        int shaderID = 0;

        try {
            shaderID = gl3.glCreateShader(type); // Allocate an ID for the shader

            File vertexFile = new File(filepath); // Open the file

            // Read the full source code.
            String vertexShaderSource = new Scanner(vertexFile).useDelimiter("\\Z").next();

            // Load the source code to OpenGL
            gl3.glShaderSource(shaderID, 1, new String[]{vertexShaderSource}, new int[]{vertexShaderSource.length()},
                    0);

            // Compile the shader code
            gl3.glCompileShader(shaderID);

            int[] shaderStatus = new int[1];

            gl3.glGetShaderiv(shaderID, GL_COMPILE_STATUS, shaderStatus, 0);

            // check whether compilation was successful
            if (shaderStatus[0] == GL_FALSE) {

                int[] length = new int[1];
                byte[] buffer = new byte[1000];

                gl3.glGetShaderInfoLog(shaderID, 1000, length, 0, buffer, 0);

                String log = new String(buffer, 0, length[0]);

                throw new IllegalStateException("compilation error for shader ["
                        + filepath + "]. Reason: " + log);
            }

            return shaderID;
        } catch (FileNotFoundException ex) {
            Logger.getLogger(ShaderProgram.class.getName()).log(Level.SEVERE, null, ex);
            System.exit(-1);
        }

        return shaderID;
    }

    /**
     * Set this matrix as the value of the specified uniform.
     * <p>
     * Note: if the shader doesn't have such a uniform, "false" is returned,
     * but no errors are thrown.
     *
     * @param name   The name of the uniform
     * @param matrix The matrix to load into OpenGL
     * @return Whether the shader actually has a uniform of that name.
     */
    public boolean setUniform(String name, Matrix4d matrix, GL3 gl) {

        float[] floats = new float[16];
        // Serialize matrix
        matrix.get(floats);

        // Retrieve modelviewproj uniform location
        int loc = gl.glGetUniformLocation(glShaderProgram, name);

        // Send the matrix to OpenGL
        gl.glUniformMatrix4fv(loc, 1, false, floats, 0);

        return loc != -1;
    }

    public boolean setUniform(String name, float f, GL3 gl3) {

        // Retrieve modelviewproj uniform location
        int loc = gl3.glGetUniformLocation(glShaderProgram, name);

        // Send the matrix to OpenGL
        gl3.glUniform1f(loc, f);

        return loc != -1;
    }

    public boolean setUniform(String name, int i, GL3 gl3) {

        // Retrieve modelviewproj uniform location
        int loc = gl3.glGetUniformLocation(glShaderProgram, name);

        // Send the matrix to OpenGL
        gl3.glUniform1i(loc, i);

        return loc != -1;
    }

    public boolean setUniform(String name, Vector3f vec4, GL3 gl3) {

        float[] floats = new float[]{vec4.x, vec4.y, vec4.z};

        // Retrieve modelviewproj uniform location
        int loc = gl3.glGetUniformLocation(glShaderProgram, name);

        // Send the matrix to OpenGL
        gl3.glUniform3fv(loc, 1, floats, 0);

        return loc != -1;
    }

    public int getUniformLocation(String name, GL3 gl3) {
        int loc = gl3.glGetUniformLocation(glShaderProgram, name);
        if (loc == -1) {
            throw new IllegalStateException("Unknown uniform " + name);
        }
        return loc;
    }

    /**
     * @return the glShaderProgram
     */
    public int getGlShaderProgram() {
        return glShaderProgram;
    }

    /**
     * Tell OpenGL to use this program when rendering.
     */
    public void use(GL3 gl3) {
        gl3.glUseProgram(glShaderProgram);
    }

}
