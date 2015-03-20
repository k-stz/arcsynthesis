#version 330

layout (location = 0) in vec4 position;
layout (location = 1) in vec4 color;

smooth out vec4 fs_colors;

uniform vec2 offset;
// portions faces between (relative to the eye!) zNear and zFar will be rendered
// remember we look down the -Z axis so zNear = 1.0 is really Z = -1
// and portions of faces with Z greater(because we look down negative axis) than -1
// won't be viewed! zNear and zFar effectifly "cut" off 3d world perpendicular to
// the Z axis, also only on the -Z axis, as our eye is at origin (0,0,0) looking down
// the -Z axis!
uniform float zNear; // must be greater than 0
uniform float zFar;
uniform float frustumScale;

void main () {
  vec4 cameraPos = position + vec4(offset.x, offset.y, 0.0, 0.0);
  vec4 clipPos;

  // scale world without changing yet keeping eye projection plane constant |-1|
  // also note cliPos.xy <- .xy does what you would intuitively expect and it does
  // it very efficiently. This is called "swizzle selection"
  clipPos.xy = cameraPos.xy * frustumScale;

  // Zclip = (Zcamera(F+n)/(N-F))  +  (2*N*F/(N-F))
  // TODO: from what I understand Zclip is = Pz i.e. the perpendicular
  // distance from any vertex to the eye-plane. There is something odd
  // hardcoding clipPos.z doesn't seem to change the program as arcsynthesis
  // suggests. Idea: probably projection divide has rules not yet fully
  // explained
  clipPos.z = cameraPos.z * (zNear + zFar) / (zNear - zFar);
  clipPos.z += 2 * zNear * zFar / (zNear - zFar);

  // Pz/Ez, Ez=-1 => Pz/-1 => -Pz
  // also note that Pz = perpendicular distance from point to eye-plane!!
  // well since eye is at origin, the provided Z = Pz!
  clipPos.w = -cameraPos.z;

  // TODO: why do x,y changing result in expected rendering while
  // changing clipPos.z cause zNear zFar like cutting???
  //  clipPos.z += 1; clipPos.x -=1;
  
  gl_Position = clipPos;
  fs_colors = color;
}
