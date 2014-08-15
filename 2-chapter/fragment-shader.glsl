#version 330

out vec4 outputColor;

void main()
{
   //awesome: gl_FragCoord stores the x y z (window coordinates) of every fragment!!!
   //oh, and it seems you can just define variables where ever you want
   // 500.0f is supposed to be the height of the window
   // meaning y = 500 / 500 = 1.0 (as in all colors are on scale from 0 to 1.0!)
   float lerpValue = gl_FragCoord.y / 400.0f;

   //so now that I understand this source code, dividing by the window-size is like
   //normalization as it scales down the fragCoord.y to the intervall [0,1] in effect
   //(if 400.0 really is the window height!)

   // whooooooooooa: linear interpolation (!!!) going on here:
   // lerp is graphics jargon for "linear interpolation"!!
   // mix() is a opengl standard function that performs linear interpolation between
   // two values! (what a dumb name for a lerp function!!!111eleven-celeb-seventeen)
   //*whelp*, it kind of mixes because its an INTERpolation between values, as in
   // meeting the middle ground (or as in colors mixing!)
   outputColor = mix(vec4(1.0f, 1.0f, 1.0f, 1.0f),
       vec4(0.2f, 0.2f, 0.2f, 1.0f), lerpValue);
}