#version 330

//TODO: if the VAO doesn't provide an attribute then the value
//      the shaders gets presumably defaults to a (0,0,0,1) vector!?
//      if vec3 defined below (0,0,0)
// the base case is: if something is lacking it will be filled with
// values in order and place: (0,0,0,1) e.g. vec2(3,5) -> (3,5,0,1)
layout(location = 0) in vec4 position;
layout(location = 1) in vec4 color;

smooth out vec4 interp_color;

uniform mat4 model_to_world_matrix;

// 'layout(..)' specifies the _uniform block layout_, like gl:buffer-data
// input, at first ubo are unformatted arrays of bytes. 'std140' tells opengl
// that the mat4 matrices are col-major, all uniforms must be stored (no default
// values if not set explicitly) and, what we want, multiple uniform programs can
// use _the same_ uniform buffer
// This is the uniform block, it is identified as "global_matrices":
layout(std140) uniform global_matrices {
  // order of these is crucial, as we will set these by using proper :buffer-offset
  // in gl:sub-buffer-data calls!
  mat4 camera_to_clip_matrix;
  mat4 world_to_camera_matrix; 
};

void main()
{
  vec4 temp = model_to_world_matrix * position;
  temp = world_to_camera_matrix * temp;

  gl_Position = camera_to_clip_matrix * temp;
  interp_color = color;
}
