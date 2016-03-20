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
  vec2 tc = fwidth(In.TexCoord) * 4.0;
  float w = max(tc.x, tc.y);
  float v = smoothstep(0.5-w, 0.5+w, tx.r);
  Color = vec4(v, v, v, 1.0);
}
