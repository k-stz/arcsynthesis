#version 330

layout (location = 0) in vec4 positions;
layout (location = 1) in vec4 colors;

smooth out vec4 fs_colors;

uniform vec2 offset;

void main () {
  // oooh, see how you access the components of offset using dot
  // notation!!
  gl_Position = positions + vec4(offset.x, offset.y, 0.0, 0.0);
  fs_colors = colors;
}
