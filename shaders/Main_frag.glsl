#version 450

varying vec2 fragmentTextureCoordinate;
uniform float fTime = 0;

void main()
{
    vec2 uv = fragmentTextureCoordinate;
    float stripe = pow(abs(sin(uv.x * 100.0 - 20.0 * uv.y + fTime * 10.0)), 16.0);
    gl_FragColor = vec4(uv, stripe, 1.0);

}