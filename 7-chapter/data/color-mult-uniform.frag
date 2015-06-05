#version 330

smooth in vec4 interp_color;
uniform vec4 base_color;

out vec4 outputColor;

void main(){

  outputColor = interp_color * base_color;
}
