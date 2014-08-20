#version 330

out vec4 outputColor;

// arcsynthesis' "time", but we need to use the same uniform as in the
// same uniform as in the vector shader: vs-calc-offset.glsl
// uniforms are "global" across all shaders, hence this uniform
// is the same as the one used in the vector-shader "vs-calc-offset.glsl"
uniform float time;

// note because uniforms are "global" if we want to use a new loop_duration
// variable we have to give it a different name since "loop_duration" stores
// already (/ sdl2:get-ticks 1000.0
uniform float frag_loop_duration;

const vec4 firstColor = vec4(0.0f, 1.0f, 1.0f, 1.0f);
const vec4 secondColor = vec4(1.0f, 0.0f, 0.0f, 1.0f);

void main() {
  float currTime = mod(time, frag_loop_duration);
  float currLerp = currTime / frag_loop_duration;

  // mix returns a value on the range [firstColor, secondColor]
  // depending on its third parameter (currLerp) which must be on the range [0.0,1.0]
  // or the outcome will be undefined!
  // also mix has the added benefit on being able to operate on vec4 types!

  // Bonus: oscilate between 0 and 1, with tricky sin 2*pi call
  outputColor = mix(firstColor, secondColor, sin(3.141592 * 2 * currLerp));
}
