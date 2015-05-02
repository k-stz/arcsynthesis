#version 330

in vec4 diffuseColor;
in vec3 vertexNormal;
in vec3 cameraSpacePosition;

out vec4 outputColor;

uniform vec3 modelSpaceLightPos;

uniform vec4 lightIntensity;
uniform vec4 ambientIntensity;

uniform vec3 cameraSpaceLightPos;

uniform float lightAttenuation;

const vec4 specularColor = vec4(0.25, 0.25, 0.25, 1.0);
uniform float shininessFactor;


float CalcAttenuation(in vec3 cameraSpacePosition, out vec3 lightDirection)
{
	vec3 lightDifference =  cameraSpaceLightPos - cameraSpacePosition;
	float lightDistanceSqr = dot(lightDifference, lightDifference);
	lightDirection = lightDifference * inversesqrt(lightDistanceSqr);
	
	return (1 / ( 1.0 + lightAttenuation * sqrt(lightDistanceSqr)));
}

void main()
{
	vec3 lightDir = vec3(0.0);
	//lightDir is implicitly set in call to CalcAttenuation!
	float atten = CalcAttenuation(cameraSpacePosition, lightDir);
	vec4 attenIntensity = atten * lightIntensity;
	
	vec3 surfaceNormal = normalize(vertexNormal);
	float cosAngIncidence = dot(surfaceNormal, lightDir);
	cosAngIncidence = clamp(cosAngIncidence, 0, 1);

	// because we're in camera space negating the current fragments
	// direction will get us the direction from the camera (0,0,0)
	// to the fragment: C - Frag_pos where C= (0,0,0) hence just -Frag_pos
	vec3 viewDirection = normalize(-cameraSpacePosition);
	vec3 reflectDir = reflect(-lightDir, surfaceNormal);
	float phongTerm = dot(viewDirection, reflectDir);
	phongTerm = clamp(phongTerm, 0, 1);
	phongTerm = cosAngIncidence != 0.0 ? phongTerm : 0.0;
	phongTerm = pow(phongTerm, shininessFactor);
	

	outputColor = (diffuseColor * attenIntensity * cosAngIncidence) +
		(specularColor * attenIntensity * phongTerm) +
		(diffuseColor * ambientIntensity);
}
