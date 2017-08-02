#version 330

in vec3 position;
in vec3 in_normal;
in vec2 texCoord;

uniform mat4 modelviewproj;
uniform mat4 modelview;

out vec3 normal;
out vec2 vtexCoord;

void main()
{
    // Transform the position to NDC coordinates
    gl_Position = modelviewproj * vec4(position,1.0);

    // Transform the normal to view space for lighting
    normal = in_normal;

    // Simply pass the texture coordinate along to the fragment shader
    vtexCoord = texCoord;
}