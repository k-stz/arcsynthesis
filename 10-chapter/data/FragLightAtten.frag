#version 330

// If you thought that calculating the polar-coordinates from two angles and
// a distance scalar through the use of trigonometery was black magic then you'll love
// this:

// Our objective is to perform the lighting model for which we need the light source
// direction to the fragment and the fragments direction to the light. Both need to
// be in camera space, well that's where we want to have them, they really just need
// to be in a common space.

// The fragment direction is given "vertexNormal" it was stored within the objects xml
// for each vertex and interpolated over the triangles surface right here, in the
// fragment shader.

// The thing that is missing is the light-direction. The light position is already
// given "cameraSpaceLightPos". So what we need to do is take the light position and 
// negate it by the fragments position to get our light-direction.
// Easy, like the vertexNormal it is stored in the objects xml "position" and can
// be interpolated over the surface of the triangle right here..

// Except that we want to cast a grand master black magic spell:
// We will derive the fragment camera space position directly from its window-space
// position --which is given by the build-in function gl_FragCoord. 
// This wizardry is called reverse transformation and is only used here by the tutorial
// to showcase that it is a thing. It is certainly less efficient because, as was explained
// the "position" could be easily interpolated from model-space straight to camera space.
// The author states that in the future there will be a real use for reverse
// transformation.

// This is what'll happens within CalcCameraSpacePosition():
//                     1. FragCoord (window-space)
//                        <arc formulas to get at NDC coordinates>
//                     2. NDC-coord
//                         inverse of projection matrix, unprojection, "clipToCameraMatrix"
//                     ==> cameraSpacePosition

in vec4 diffuseColor;
in vec3 vertexNormal;

out vec4 outputColor;

uniform vec3 modelSpaceLightPos;

uniform vec4 lightIntensity;
uniform vec4 ambientIntensity;

uniform vec3 cameraSpaceLightPos;

uniform float lightAttenuation;
uniform bool bUseRSquare;

uniform UnProjection
{
    mat4 clipToCameraMatrix;
    ivec2 windowSize;
};

vec3 CalcCameraSpacePosition()
{
	vec4 ndcPos;
	ndcPos.xy = ((gl_FragCoord.xy / windowSize.xy) * 2.0) - 1.0;
	ndcPos.z = (2.0 * gl_FragCoord.z - gl_DepthRange.near - gl_DepthRange.far) /
		(gl_DepthRange.far - gl_DepthRange.near);
	ndcPos.w = 1.0;
	
	vec4 clipPos = ndcPos / gl_FragCoord.w;
	
	return vec3(clipToCameraMatrix * clipPos);
}

vec4 ApplyLightIntensity(in vec3 cameraSpacePosition, out vec3 lightDirection)
{
	vec3 lightDifference =  cameraSpaceLightPos - cameraSpacePosition;
	// don't miss this math gold: dot= ||A||*||B||*cos(theta)
	// and if A equals B, then there is no angle between them
	// theta = 0 -> cos(0) = 1 -> "||A||*||B||" is indeed the distance squared!
	// How useful, since need the squared distance for the inverse-square attenuation!
	float lightDistanceSqr = dot(lightDifference, lightDifference);
	lightDirection = lightDifference * inversesqrt(lightDistanceSqr);
	
	float distFactor = bUseRSquare ? lightDistanceSqr : sqrt(lightDistanceSqr);

	return lightIntensity * (1 / ( 1.0 + lightAttenuation * distFactor));
}

void main()
{
	vec3 cameraSpacePosition = CalcCameraSpacePosition();

	vec3 lightDir = vec3(0.0);
	vec4 attenIntensity = ApplyLightIntensity(cameraSpacePosition, lightDir);

	float cosAngIncidence = dot(normalize(vertexNormal), lightDir);
	cosAngIncidence = clamp(cosAngIncidence, 0, 1);
	
	outputColor = (diffuseColor * attenIntensity * cosAngIncidence) +
		(diffuseColor * ambientIntensity);
}
