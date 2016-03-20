#version 330
  
uniform sampler2DArray Texture;   // The texture sampler.
uniform vec3 BgColor;             // RGB background color. 
uniform vec3 FgColor;             // RGB foreground color. 

in block
{
  vec3 TexCoord;          // UVW interpolated texture coordinates.
} In;

layout(location = 0, index = 0) out vec4 Color;              

void main()
{    
  vec4 tx = texture(Texture, In.TexCoord);
  vec2 tc = fwidth(In.TexCoord.xy) * 4.0;
  float w = max(tc.x, tc.y);
  vec3 c = mix(BgColor, FgColor, step(0.5-w, tx.r));
  float a = smoothstep(0.5-w, 0.5+w, tx.r);
  Color = vec4(c.r, c.g, c.b, a);
}
