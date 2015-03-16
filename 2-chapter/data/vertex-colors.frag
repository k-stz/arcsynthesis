#version 330

// the "opposite" of smooth is: flat! (no interpolation!)
smooth in vec4 theColor; // alright, first place!

out vec4 outputColor;

void main() {
    outputColor = theColor;
}
   