#version 420 core

layout(location=0) in vec3 rcoord2d;

//uniform mat3 toVP;

void main(void) {
  //vec3 vpos = toVP * rcoord2d;
  vec3 vpos = rcoord2d;
  gl_Position = vec4(vpos.xy, 0., vpos.z);
}
