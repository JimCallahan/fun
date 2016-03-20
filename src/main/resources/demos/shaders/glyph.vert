#version 330

uniform mat4 MvpMatrix;                 // The Model-View-Projection matrix.

layout(location = 0) in vec2 Position;  // 2D position of vertex.
layout(location = 1) in vec3 TexCoord;  // UVW texture coordinate of vertex.
layout(location = 2) in vec3 FgColor;   // RGB foreground color. 
 
out block 
{
  vec3 TexCoord;                        // UVW interpolated texture coordinates.
  vec3 FgColor;                         // RGB interpolated foreground color. 
} Out;
 
void main()
{
  Out.TexCoord = TexCoord;
  Out.FgColor = FgColor;
  gl_Position = MvpMatrix * vec4(Position, 0.0, 1.0);
}
