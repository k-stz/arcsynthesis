#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 color;

smooth out vec4 theColor;

// perspective matrix
uniform mat4 camera_to_clip_matrix;
// stores the transform matrix
uniform mat4 model_to_camera_matrix;

void main()
{
  // actuall translation takes place in this line:
    vec4 cameraPos = model_to_camera_matrix * position;
  //k-stz test:
  //  mat4 id;
  // id[0] = vec4( 0.0, -1.0, 0.0, 0.0); // x
  // id[1] = vec4( -1.0, 0.0, 0.0, 0.0); // y
  // id[2] = vec4( 0.0, 0.0, 1.0, 0.0); // z
  // id[3] = vec4( 0.0, 0.0, 0.0, 1.0); // w
  // cameraPos = id * cameraPos;
    gl_Position = camera_to_clip_matrix * cameraPos;
    theColor = color;
}
