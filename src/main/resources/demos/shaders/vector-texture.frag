#version 330
  
uniform sampler2D Texture;   // The texture sampler.

in block
{
  vec2 TexCoord;          // UV interpolated texture coordinates.
} In;

layout(location = 0, index = 0) out vec4 Color;              

void main()
{    
  vec4 tx = texture(Texture, In.TexCoord);
  float v = step(0.5, tx.r);
  Color = vec4(v, v, v, 1.0);
}
