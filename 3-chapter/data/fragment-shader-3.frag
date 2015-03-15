#version 330

// 3 vertices;many fragments
smooth in vec4 colorValue;
out vec4 outputColor; 

void main() {
  outputColor = colorValue;
}