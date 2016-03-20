#version 330

uniform mat4 MvpMatrix;                 // The Model-View-Projection matrix.

layout(location = 0) in vec2 Position;  // 2D position of vertex.
layout(location = 1) in vec3 TexCoord;  // UVW texture coordinate of vertex.
 
out block 
{
  vec3 TexCoord;                        // UVW interpolated texture coordinates.
} Out;
 
void main()
{
  Out.TexCoord = TexCoord;
  gl_Position = MvpMatrix * vec4(Position, 0.0, 1.0);
}
