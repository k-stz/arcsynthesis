#version 330

in vec4 diffuseColor;
in vec3 vertexNormal;
in vec3 cameraSpacePosition;

out vec4 outputColor;

layout(std140) uniform;

uniform Material
{
	vec4 diffuseColor;
	vec4 specularColor;
	float specularShininess;
} Mtl; // this is the _instance name_ we can use it to access values
       // in the UBO like so: Mtl.diffuseColor (see below) without this
       // the contents would be global and we'd had problems with the
       // "in vec4 diffuseColor" definition above!

struct PerLight
{
	vec4 cameraSpaceLightPos;
	vec4 lightIntensity;
};

const int numberOfLights = 4;

// Paraphrasing: 7.6.2.2 Standard Uniform Block Layout of the
// OpenGL (Core profile) specification
// The uniform block gets its values from a buffer. In this case the
// *light-uniform-buffer* (in scene-lighting.lisp). But the offsets assigned to each
// uniform are implementation-dependent (of the driver). OpenGL provides functions
// to querry the offsets assigned to uniforms in uniform blocks at runtime!
// Using the 'layout' qualifier, here: layout(std140) uniform; the offsets of each uniform
// can be derived from the order of definitions in the uniform block!

// On std140,
// members of the struct will be stored in the same order they appear in the uniform block
// they will usually require their own size as a an allignment offset multiple. PerLight
// base allignment is that of its largest memeber: a vec4 (for example the
// cameraSpaceLightPos). So a multiple of 16byte is required:
uniform Light
{
  vec4 ambientIntensity; //16 bytes
  float lightAttenuation; //just 4 bytes, but PerLight needs a multiple of 16 bytes allignment
  // so here we had the struct add padding [3] and thus the shader will read from the
  // *light-uniform-buffer* the 32nd byte and extract the PerLight values!
  PerLight lights[numberOfLights];
} Lgt;


float CalcAttenuation(in vec3 cameraSpacePosition,
	in vec3 cameraSpaceLightPos,
	out vec3 lightDirection)
{
	vec3 lightDifference =  cameraSpaceLightPos - cameraSpacePosition;
	float lightDistanceSqr = dot(lightDifference, lightDifference);
	lightDirection = lightDifference * inversesqrt(lightDistanceSqr);
	
	return (1 / ( 1.0 + Lgt.lightAttenuation * lightDistanceSqr));
}

vec4 ComputeLighting(in PerLight lightData)
{
	vec3 lightDir;
	vec4 lightIntensity;
	if(lightData.cameraSpaceLightPos.w == 0.0)
	{
		lightDir = vec3(lightData.cameraSpaceLightPos);
		lightIntensity = lightData.lightIntensity;
	}
	else
	{
		float atten = CalcAttenuation(cameraSpacePosition,
			lightData.cameraSpaceLightPos.xyz, lightDir);
		lightIntensity = atten * lightData.lightIntensity;
	}
	
	vec3 surfaceNormal = normalize(vertexNormal);
	float cosAngIncidence = dot(surfaceNormal, lightDir);
	cosAngIncidence = cosAngIncidence < 0.0001 ? 0.0 : cosAngIncidence;
	
	vec3 viewDirection = normalize(-cameraSpacePosition);
	
	vec3 halfAngle = normalize(lightDir + viewDirection);
	float angleNormalHalf = acos(dot(halfAngle, surfaceNormal));
	float exponent = angleNormalHalf / Mtl.specularShininess;
	exponent = -(exponent * exponent);
	float gaussianTerm = exp(exponent);

	gaussianTerm = cosAngIncidence != 0.0 ? gaussianTerm : 0.0;
	
	vec4 lighting = diffuseColor * lightIntensity * cosAngIncidence;
	lighting += Mtl.specularColor * lightIntensity * gaussianTerm;
	
	return lighting;
}

void main()
{
	vec4 accumLighting = diffuseColor * Lgt.ambientIntensity;
	for(int light = 0; light < numberOfLights; light++)
	{
		accumLighting += ComputeLighting(Lgt.lights[light]);
	}
	
	outputColor = accumLighting;
}
