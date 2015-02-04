#version 330

smooth in vec4 theColor;
uniform vec4 base_color;

out vec4 outputColor;

void main() {
 outputColor = theColor * base_color;
}
