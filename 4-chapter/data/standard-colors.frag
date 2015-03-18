#version 330

smooth in vec4 fs_colors;
out vec4 out_color;

void main() {
    out_color = fs_colors;
}
