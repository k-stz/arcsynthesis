#version 330

layout(location = 0) in vec4 position;
// note: uniform is neither 'in' nor 'out' variable!
uniform vec2 offset;

void main()
{
    // note the neat vec4() function returning a vec4 vector!
    // oh oh ooooh it is NOT a function it is a CONSTRUCTOR !!!!!11
    vec4 totalOffset = vec4(offset.x, offset.y, 0.0, 0.0);

    //  GLSL has a lot of vector math built in! But we can't add vectors
    //  of different dimensions (vec2 + vec4) THAT'S WHY we had to construct
    //  a new one using vec4( ..,..,..,..)   ;D

    // and check out this cool thing here! position and totalOffset
    // are both vec4 type, so we in effect do vector mathamatics here!
    // adding each component to the corresponding component of the other!!!
                 /* COMPONENT-WISE ADDITION IS IMMINENT */
    gl_Position = position + totalOffset;
}