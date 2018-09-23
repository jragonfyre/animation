#version 420 core

in vec4 gl_FragCoord;
out vec4 gl_FragColor;

uniform vec4 solidColor;

void main(void) {
  //gl_FragColor = vec4(0., 0., 1., floor(mod(gl_FragCoord.y, 2.)));
  gl_FragColor = solidColor;
}


