#version 330


layout(location = 0) in vec4 positions;

uniform float time;
uniform float loop_duration;

void main(){
 float time_scale = (3.14159 * 2) / loop_duration;
 float curr_time_through_loop = mod(time, loop_duration);
 
 // set x/y-offsets
 float x_offset = cos(curr_time_through_loop * time_scale) * 0.5;
 float y_offset = sin(curr_time_through_loop * time_scale) * 0.5;
 
 // for vector mathematics component-wise addition ;)
 vec4 offsets = vec4(x_offset, y_offset, 0.0, 0.0);

 gl_Position = positions + offsets;
}
