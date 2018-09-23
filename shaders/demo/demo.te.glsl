#version 420
#extension GL_ARB_tessellation_shader : enable

layout(triangles, equal_spacing, ccw) in;

in vec3 gl_TessCoord;

uniform vec2 baseVert;
uniform float t;

vec4 evaluate(float t) 
{
  vec4 e11 = mix(gl_in[0].gl_Position,gl_in[1].gl_Position,t);
  vec4 e21 = mix(gl_in[1].gl_Position,gl_in[2].gl_Position,t);
  vec4 e31 = mix(gl_in[2].gl_Position,gl_in[3].gl_Position,t);
  vec4 e12 = mix(e11,e21,t);
  vec4 e22 = mix(e21,e31,t);
  return mix(e12,e22,t);
}

void main()
{
  vec4 posTemp=vec4(0.);
  if(gl_TessCoord.x == 0.)
  {
    posTemp = evaluate(gl_TessCoord.z);
  } else
  {
    posTemp = gl_in[4].gl_Position;
    //gl_Position = vec4(baseVert,0.,1.);
  }
  vec2 displacement = 0.05*vec2(cos(t+6*posTemp.x),sin(t+6*posTemp.y));
  mat4 transform = mat4(vec4(1.,0.,0.,0.),vec4(0.,1.,0.,0.),vec4(0.,0.,0.,1.),vec4(displacement,0.,1.));
  gl_Position = transform*posTemp;
  /*
  gl_Position =
      (gl_TessCoord.x) * gl_in[0].gl_Position +
      gl_TessCoord.y * gl_in[1].gl_Position + 
      gl_TessCoord.z * gl_in[2].gl_Position;
  */
}
