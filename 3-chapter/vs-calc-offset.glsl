#version 330


layout(location = 0) in vec4 positions;
// for some color to the fragment shader:
layout(location = 1) in vec4 attrib_color;
smooth out vec4 colorValue;
uniform float sdl_ticks;
uniform float loop_duration;

void main(){
 // more color to the fragment shader
 colorValue = attrib_color;

 float scale = (3.14159 * 2) / loop_duration;
 float elapsed_time = sdl_ticks / 1000.0;
 float curr_time_through_loop = mod(elapsed_time, loop_duration);
 
 // set x/y-offsets
 float x_offset = cos(curr_time_through_loop * scale) * 0.5;
 float y_offset = sin(curr_time_through_loop * scale) * 0.5;
 
 // for vector mathematics component-wise addition ;)
 vec4 offsets = vec4(x_offset, y_offset, 0.0, 0.0);

 gl_Position = positions + offsets;
}