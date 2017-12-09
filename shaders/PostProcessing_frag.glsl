#version 430

varying vec2 screenUV;
varying vec2 screenXY;
uniform float fTime = 0;
uniform vec2 fMouse = ivec2(0, 0);

uniform sampler2D sceneTexture;

void main()
{
    //float stripe = pow(abs(sin(screenUV.x * 100.0 - 20.0 * screenUV.y + fTime * 10.0)), 16.0);
    //vec4 finalColor = texture(sceneTexture, screenUV) + vec4(screenUV * 0.1, stripe, 0);
    gl_FragColor = pow(texture(sceneTexture, screenUV), vec4(1.0/2.2)); // gamma corrected
}