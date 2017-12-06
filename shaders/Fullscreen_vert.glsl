#version 450

out vec2 fragmentTextureCoordinate;
 
void main()
{
    vec2 uv = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
    fragmentTextureCoordinate = uv;
    gl_Position = vec4(uv * 2.0f - 1.0f, 0, 1);
}
