#version 330

layout (location = 0) in vec4 position;
layout (location = 1) in vec4 color;

// smooth. is an INTERPOLATION QUALIFIER wooOHOHOHOwhoWHOWHOWHOWH
// the closer a fragment is to one vertex, the more that vertex's output
// contributes to the value of the fragment PROGRAM receives (and hence
// the current fragment being run through it!) remember this shader
// is executed once PER fragment (and there are as many fragments as pixels
// cover a particular primitive, in this case a triangle) so in this case
// about more than 20.000~ times!

// secondly, such interpolation is the most common input type for a fragment
// shader of vertex shader variable, that's why it is used by default if no
// keyword is provided!
smooth out vec4 theColor;

void main () {
    gl_Position = position;
    theColor = color;
}