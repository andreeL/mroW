#version 450

out vec2 screenUV;
out vec2 screenXY;
uniform vec2 fFov = vec2(1.0, 1.0);

void main()
{
    screenUV = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
    screenXY = (screenUV - 0.5) * fFov * 2.0;
    gl_Position = vec4(screenUV * 2.0f - 1.0f, 0, 1);
}
