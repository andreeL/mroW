#version 450

varying vec2 fragmentTextureCoordinate;

void main()
{
    gl_FragColor = vec4(fragmentTextureCoordinate, .1f, 1.f);
}