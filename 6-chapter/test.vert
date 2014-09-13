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
  vec4 cameraPos =  model_to_camera_matrix * position;
  //k-stz test:
  mat4 sm, tm, sm2, tm2;
  sm[0] = vec4( 1.5, 0.0, 0.0, 0.0);  ;sm2[0] = vec4( 0.5, 0.0, 0.0, 0.0); // x
  sm[1] = vec4( 0.0, 1.5, 0.0, 0.0);  ;sm2[1] = vec4( 0.0, 0.5, 0.0, 0.0); // y
  sm[2] = vec4( 0.0, 0.0, 1.0, 0.0);  ;sm2[2] = vec4( 0.0, 0.0, 1.0, 0.0); // z
  sm[3] = vec4( 0.0, 0.0, 0.0, 1.0);  ;sm2[3] = vec4( 0.0, 0.0, 0.0, 1.0); // w

  tm[0] = vec4( 1.0, 0.0, 0.0, 0.0); tm2[0] = vec4( 1.0, 0.0, 0.0, 0.0);
  tm[1] = vec4( 0.0, 1.0, 0.0, 0.0); tm2[1] = vec4( 0.0, 1.0, 0.0, 0.0);
  tm[2] = vec4( 0.0, 0.0, 1.0, 0.0); tm2[2] = vec4( 0.0, 0.0, 1.0, 0.0);
  tm[3] = vec4( 0.5, 0.5, 0.0, 1.0); tm2[3] = vec4( -1.0, 0.0, 0.0, 1.0);
  // when we abide to the order: translation * rotation * scaling
  // we can perform relative transformation as is the intent of the
  // hierarchical model.
  // Therefore new transformation must always abide this order:
  // tm * new-tm * old-s even-newer-s new-s
  // abiding to this rule exposes a minor extend of commutative behavior:
  // matrices of the same kind can be multiplied in any order
  // its own kind are these: translation, scale, rotation -matrices!!
  cameraPos = tm * tm2 * sm2 * sm * cameraPos;
  //test ovaaaa
  gl_Position = camera_to_clip_matrix * cameraPos;
    theColor = color;
}
