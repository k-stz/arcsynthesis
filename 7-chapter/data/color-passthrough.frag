#version 330

smooth in vec4 interp_color;

out vec4 outputColor;

void main()
{
	outputColor = interp_color;
}
