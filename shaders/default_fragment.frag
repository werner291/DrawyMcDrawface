#version 330

varying vec3 normal;
varying vec2 vtexCoord;

uniform vec3 lightDir;

uniform sampler2D texture;

uniform vec3 mat_ambient;
uniform vec3 mat_diffuse;
uniform vec3 mat_specular;
uniform vec3 mat_emissive;

void main(){

    // Normalize the light direction
    vec3 lightDirection = normalize(vec3(0.1,1,0));//normalize(lightDir).xyz;

    // Compute diffuse brightness as the cosine of the angle between the light direction and the normal
    float diffuseBrightness = dot(normal,lightDirection);

    // Sample the texture
    vec4 color = texture2D(texture, vtexCoord);

    // Combine the lighting intensity with the material colors
    vec4 materialColor = vec4(mat_ambient + mat_diffuse * diffuseBrightness,1);

    // Combine the material with the texture
    gl_FragColor = vec4(diffuseBrightness,diffuseBrightness,diffuseBrightness,1);
}
