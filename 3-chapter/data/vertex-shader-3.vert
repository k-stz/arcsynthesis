#version 330

layout (location = 0) in vec4 position;
layout (location = 1) in vec4 colorInput;

smooth out vec4 colorValue;

void main() {
 gl_Position = position;
 colorValue = colorInput;
}