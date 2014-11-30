#version 330

//TODO: if the VAO doesn't provide an attribute then the value
//      the shaders gets presumably defaults to a (0,0,0,1) vector!?
//      if vec3 defined below (0,0,0)
// the base case is: if something is lacking it will be filled with
// values in order and place: (0,0,0,1) e.g. vec2(3,5) -> (3,5,0,1)
layout(location = 0) in vec4 position;

uniform mat4 model_to_world_matrix;

layout(std140) uniform global_matrices {
 mat4 camera_to_clip_matrix;
 mat4 world_to_camera_matrix; 
};


void main()
{
  vec4 temp = model_to_world_matrix * position;
  temp = world_to_camera_matrix * temp;

  gl_Position = camera_to_clip_matrix * temp;
}
