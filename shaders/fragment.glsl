#version 330

varying vec3 normal;
varying vec2 vtexCoord;

uniform vec3 lightDir;

uniform sampler2D texture;

uniform vec4 mat_ambient;
uniform vec4 mat_diffuse;
uniform vec4 mat_specular;
uniform vec4 mat_emissive;

void main(){

    // Normalize the light direction
    vec3 lightDirection = normalize(vec3(0.2,0.2,1.0));//normalize(lightDir).xyz;

    // Compute diffuse brightness as the cosine of the angle between the light direction and the normal
    float diffuseBrightness = dot(normal,lightDirection);

    // Sample the texture
    vec4 color = texture2D(texture, vtexCoord);

    // Combine the material with the texture
    //gl_FragColor = /*mat_ambient + mat_diffuse **/ vec4(diffuseBrightness;
    gl_FragColor = vec4(diffuseBrightness,diffuseBrightness,diffuseBrightness,1.0);
}