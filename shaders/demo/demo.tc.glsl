#version 420
#extension GL_ARB_tessellation_shader : enable

//in int gl_PatchVerticesIn;


layout (vertices = 5) out;

uniform mat3 toVP;

void main(void)
{
  
  vec4 pos = gl_in[gl_InvocationID].gl_Position;
  float subdivHint = gl_in[4].gl_Position.w;
  if(gl_InvocationID == 4)
  {
    pos.w=1; // we've extracted the subdiv hint, set the coordinate to the proper value
  }
  vec3 post = toVP * pos.xyw;
  
  if(subdivHint > 0.5)
  {
    gl_TessLevelOuter[0]=subdivHint;
  } else
  {
    gl_TessLevelOuter[0]=8.;
  }
  
  //vec3 post = gl_in[gl_InvocationID].gl_Position.xyw;
  //gl_TessLevelOuter[0]=4.;
  gl_TessLevelOuter[1]=1.;
  gl_TessLevelOuter[2]=1.;

  gl_TessLevelInner[0]=1.;
  gl_out[gl_InvocationID].gl_Position = vec4(post.xy,0.,post.z);
}

