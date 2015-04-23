#version 330

// the special thing about fragment shaders is that they will be run for each fragment
// inside a triangle, and hence the fragment shader will take care of inferring the
// position for each fragment (basically through interpolation). All we thence do is apply
// the lighting model to this fragment through the angle of incidence between its
// vertexNormal and the direction between its position and the uniform-provided
// light-source position (modelSpaceLightPos)
// for each of the "in" qualified vertex attributes interpolation takes place:
// diffusecolor: what color to use for the fragment
// vertexNormal: the normal at a certain fragment, interpolated by the triangles vertex normals
//               just like all the other values are interpolated
// modelSpacePosition: what geometricall position has the each individual fragment of the triangle


in vec4 diffuseColor;
in vec3 vertexNormal;
in vec3 modelSpacePosition;

out vec4 outputColor;

uniform vec3 modelSpaceLightPos;

uniform vec4 lightIntensity;
uniform vec4 ambientIntensity;

void main()
{
	vec3 lightDir = normalize(modelSpaceLightPos - modelSpacePosition);

	float cosAngIncidence = dot(normalize(vertexNormal), lightDir);
	cosAngIncidence = clamp(cosAngIncidence, 0, 1);
	
	outputColor = (diffuseColor * lightIntensity * cosAngIncidence) +
		(diffuseColor * ambientIntensity);
}
