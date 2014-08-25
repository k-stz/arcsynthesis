#version 330

layout (location = 0) in vec4 positions;
layout (location = 1) in vec4 colors;

uniform mat4 perspective_matrix;
uniform vec2 offset;

smooth out vec4 fs_colors;

vec4 cameraPos;

void main() {
  fs_colors = colors;
  
  cameraPos = positions + vec4(offset.x, offset.y, 0.0, 0.0);
  // beware! Matrix * vector multiplication is NOT commutative
  gl_Position =  perspective_matrix * cameraPos;
}
