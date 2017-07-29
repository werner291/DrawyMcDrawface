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
import com.jogamp.opengl.util.GLBuffers;
import nl.wernerkroneman.Drawy.ConcreteModelling.Mesh;
import org.joml.Vector2d;
import org.joml.Vector3d;

import java.nio.FloatBuffer;

import static com.jogamp.opengl.GL.*;

public class GlMesh extends Mesh {

    private int vertexBufferID = -1;
    private int vertexArrayID = -1;
    private boolean dirty = true;
    private int verticesInBuffer;

    @Override
    public void addTriangle(Vector3d a, Vector3d b, Vector3d c, Vector3d aN, Vector3d bN, Vector3d cN) {
        dirty = true;
        super.addTriangle(a, b, c, aN, bN, cN);
    }

    public void recomputeVBO(GL3 gl) {

        dirty = false;

        // Normal or texCoords arrays being empty means it has no texture or normals, but cannot
        // have different number of texture/normls as vertices.
        assert (getNormals().isEmpty() || getNormals().size() == getVertices().size());
        assert (getTexCoords().isEmpty() || getTexCoords().size() == getVertices().size());

        // ---------------------------------------
        // Compute the number of floats per vertex
        // ---------------------------------------
        int floatsPerVertex = 3;

        if (!getNormals().isEmpty()) {
            floatsPerVertex += 3;
        }

        if (!getTexCoords().isEmpty()) {
            floatsPerVertex += 2;
        }

        ////////////
        // Buffer //
        ////////////

        if (getVertexBufferID() == -1) {
            // This will identify our vertex buffer
            int[] vertexbufferArray = new int[1];
            // Generate 1 buffer, put the resulting identifier in vertexbuffer
            gl.glGenBuffers(1, vertexbufferArray, 0);
            vertexBufferID = vertexbufferArray[0];
        }

        float[] bufData = new float[getVertices().size() * floatsPerVertex];

        int bufPos = 0;

        for (int i = 0; i < getVertices().size(); i++) {

            Vector3d vertex = getVertices().get(i);

            // Write position, normal and texcoord data in interleaved format

            bufData[bufPos++] = (float) vertex.x;
            bufData[bufPos++] = (float) vertex.y;
            bufData[bufPos++] = (float) vertex.z;

            if (!getNormals().isEmpty()) {

                Vector3d normal = getNormals().get(i);

                bufData[bufPos++] = (float) normal.x;
                bufData[bufPos++] = (float) normal.y;
                bufData[bufPos++] = (float) normal.z;
            }

            if (!getTexCoords().isEmpty()) {
                Vector2d texCoord = getTexCoords().get(i);

                bufData[bufPos++] = (float) texCoord.x;
                bufData[bufPos++] = (float) texCoord.y;
            }

        }

        // The following commands will talk about our 'vertexbuffer' buffer
        gl.glBindBuffer(GL_ARRAY_BUFFER, getVertexBufferID());

        FloatBuffer buffer = GLBuffers.newDirectFloatBuffer(bufData);

        // Give our vertices to OpenGL.
        gl.glBufferData(GL_ARRAY_BUFFER, bufData.length * Float.BYTES, buffer, GL_STATIC_DRAW);

        /////////////////////////
        // Vertex array object //
        /////////////////////////

        if (getVertexArrayID() == -1) {
            int[] arrays = new int[1];
            gl.glGenVertexArrays(1, arrays, 0);
            vertexArrayID = arrays[0];
        }

        gl.glBindVertexArray(getVertexArrayID());

        gl.glEnableVertexAttribArray(ShaderPositions.POSITION);

        gl.glVertexAttribPointer(ShaderPositions.POSITION, 3, GL_FLOAT, false, floatsPerVertex * Float.BYTES, 0);

        gl.glEnableVertexAttribArray(ShaderPositions.NORMAL);

        gl.glVertexAttribPointer(ShaderPositions.NORMAL, 3, GL_FLOAT, false, floatsPerVertex * Float.BYTES, 3 * Float
                .BYTES);

        /*int offset = 6 * Float.BYTES;

        if (!colors.isEmpty()) {
            gl.glEnableVertexAttribArray(ShaderPositions.TEXCOORD);

            gl.glVertexAttribPointer(ShaderPositions.NORMAL, 2, GL_FLOAT, false, floatsPerVertex * Float.BYTES, offset);

            offset += 2 * Float.BYTES;
        }

        if (!texCoords.isEmpty()) {
            gl.glEnableVertexAttribArray(ShaderPositions.COLOR);

            gl.glVertexAttribPointer(ShaderPositions.NORMAL, 3, GL_FLOAT, false, floatsPerVertex * Float.BYTES, offset);

            offset += 3 * Float.BYTES;
        }*/

        verticesInBuffer = getVertices().size();

        gl.glBindVertexArray(0);

        gl.glBindBuffer(GL_ARRAY_BUFFER, 0);
    }

    public int getVertexBufferID() {
        return vertexBufferID;
    }

    public int getVertexArrayID() {
        return vertexArrayID;
    }

    public boolean isDirty() {
        return dirty;
    }

    public int getVerticesInBuffer() {
        return verticesInBuffer;
    }
}
