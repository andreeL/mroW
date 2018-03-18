#version 450

varying vec2 screenUV;
varying vec2 screenXY;
uniform float fTime = 0;
uniform vec3 eyePosition;
uniform mat3 eyeRotation;
uniform vec3 playerPosition;
uniform vec4[100] objects;

uniform sampler2D catTexture;

const float epsilon = 0.005;
const float maxDistance = 30.f; // TODO: set this to something sensible

vec3 surroundingDisplacement = vec3(0.5, 0, fTime * -1);

struct Scene
{
    // closest cube
    vec3 mClosestVertexPosition;
    float mClosestVertexDistance;

    // surrounding worm hole (cylinder)
    float mCylinderDistance;

    // player
    float mPlayerDistance;

    // the closest custom object
    int mClosestObjectId;
    float mClosestObjectDistance;
};

void getClosestObject(in vec3 position, out int closestObjectId, out float closestObjectDistance)
{
    closestObjectId = int(min(max(0, position.z + float(objects.length() + 1) / 2.0), objects.length() - 1)) + 0;
    const vec4 object = objects[closestObjectId];
    const vec3 objectPosition = object.xyz;

    switch (int(object.w))
    {
        case 1:
            closestObjectDistance = length(objectPosition - position) - 0.5;
            break;
        
        case 2:
            closestObjectDistance = length(objectPosition - position) - 0.45;
            break;

        default:
            closestObjectDistance = 1.0 / 0.000001; // I want to do 1/0, but that gives an annoying warning
    }
}

float getEllipsoidDistance(in vec3 relativePosition, in vec3 radius)
{
    return (length(relativePosition / radius) - 1.0) * min(min(radius.x, radius.y), radius.z);
}

vec3 getRelativePlayerPosition(in vec3 position)
{
    return playerPosition - position;
}

vec3 getRelativeHeadPosition(in vec3 position)
{
    return getRelativePlayerPosition(position) + vec3(0.012 * sin(6.5 * fTime), 0.2, 0);
}

float getPlayerHeadDistance(in vec3 position)
{
    const vec3 relativeHeadPosition = getRelativeHeadPosition(position);
    const float ear1 = getEllipsoidDistance(relativeHeadPosition + vec3(-0.06, 0.09, 0), vec3(0.03, 0.05, 0.03));
    const float ear2 = getEllipsoidDistance(relativeHeadPosition + vec3(0.06, 0.09, 0), vec3(0.03, 0.05, 0.03));
    return min(min(ear1, ear2), length(relativeHeadPosition) - 0.1);
}

float getPlayerDistance(in vec3 position) {
    const vec3 relativePosition = getRelativePlayerPosition(position);
    const float body = getEllipsoidDistance(relativePosition, vec3(0.09, 0.13, 0.09));
    const float arm1 = getEllipsoidDistance(relativePosition + vec3(-0.1, 0.08 + 0.01 * sin(7 * fTime), 0), vec3(0.05, 0.025, 0.025));
    const float arm2 = getEllipsoidDistance(relativePosition + vec3(0.1, 0.08 + 0.01 * sin(8 * fTime), 0), vec3(0.05, 0.025, 0.025));
    const float leg1 = getEllipsoidDistance(relativePosition + vec3(-0.1, -0.08 + 0.01 * sin(10 * fTime), 0), vec3(0.05, 0.025, 0.025));
    const float leg2 = getEllipsoidDistance(relativePosition + vec3(0.1, -0.08 + 0.01 * sin(9 * fTime), 0), vec3(0.05, 0.025, 0.025));
    const float tail = getEllipsoidDistance(relativePosition + vec3(0.0, -0.1, 0), vec3(0.025, 0.2, 0.025));
    const float limbs = min(min(arm1, arm2), min(leg1, leg2));
 
    return min(min(body, getPlayerHeadDistance(position)), min(limbs, tail));
}

void getPlayerMaterial(in vec3 position, in float closestDistance, out vec3 albedo, out float roughness, out float metallic)
{
    vec2 catTextureUV;
    if (closestDistance == getPlayerHeadDistance(position))
    {
        const vec3 relativeHeadPosition = normalize(getRelativeHeadPosition(position));
        catTextureUV = relativeHeadPosition.xy * 0.25 + 0.25;
        if (relativeHeadPosition.z > 0)
            catTextureUV = vec2(1 - catTextureUV.x, catTextureUV.y);
    }
    else
    {
        const vec3 relativePlayerPosition = normalize(getRelativePlayerPosition(position));
        catTextureUV = relativePlayerPosition.xy * 0.25 + vec2(0.25, 0.75);
        if (relativePlayerPosition.z > 0)
            catTextureUV = vec2(1 - catTextureUV.x, catTextureUV.y);
    }

    albedo = texture2D(catTexture, catTextureUV).rgb;
    roughness = 0.95;
    metallic = 0;
}

void getScene(in vec3 position, out Scene scene)
{
    scene.mClosestVertexPosition = fract(position + surroundingDisplacement) - 0.5;
    scene.mClosestVertexDistance = length(scene.mClosestVertexPosition) - 0.01;

    // const vec3 qubeHalfDistance = vec3(0.125, 0.025, 0.125);
    // scene.mClosestVertexPosition = fract(position + surroundingDisplacement) - 0.5;
    // scene.mClosestVertexDistance = length(max(abs(scene.mClosestVertexPosition) - qubeHalfDistance,0.0));

    float cylinderExtent = 2.0;
    scene.mCylinderDistance = cylinderExtent - length(position.xy);

    scene.mPlayerDistance = getPlayerDistance(position);

    getClosestObject(position, scene.mClosestObjectId, scene.mClosestObjectDistance);
}

float getMinimumDistance(in Scene scene)
{
    return min(
            min(scene.mClosestVertexDistance, scene.mClosestObjectDistance),
            min(scene.mCylinderDistance, scene.mPlayerDistance)
    );
}

float getDistance(in vec3 position)
{
    Scene scene;
    getScene(position, scene);
    return getMinimumDistance(scene);
}

vec3 getNormal(in vec3 position)
{
    // TODO: how many sample points do we want to use?
    // TODO: maybe try sample randomly
    // For now we just keep it simple and pick one sample in each axis direction (+x, -x, +y, -y, +z, -z)
    vec2 epsilon = vec2(0.0, epsilon);
    return normalize(vec3(
        getDistance(position + epsilon.yxx) - getDistance(position - epsilon.yxx),
        getDistance(position + epsilon.xyx) - getDistance(position - epsilon.xyx),
        getDistance(position + epsilon.xxy) - getDistance(position - epsilon.xxy)
    ));
}

float getAmbientOcclusion(in vec3 position, in vec3 normal)
{
    float distance = epsilon * 10.0;
    for (int i = 0; i < 6; ++i)
        distance = distance + (0.25 + i / 6) * getDistance(position + normal * distance);
    
    return clamp(4 * distance, 0.0, 1.0);
}

void getObjectMaterial(in int objectId, in vec3 position, out vec3 normal, out vec3 albedo, out float roughness, out float metallic)
{
    vec4 object = objects[objectId];
    vec3 objectPosition;
    normal = getNormal(position);
    switch (int(object.w))
    {
        case 1:
            albedo = vec3(1.0, 1.0, 0.0);
            roughness = 0.1;
            metallic = 1;
            break;

        case 2:
            albedo = vec3(1.0, 1.0, 0.0);
            roughness = 0.1;
            metallic = 1;
            break;

        default: // we never get here
            albedo = vec3(1.0, 1.0, 0.0);
            roughness = 0.1;
            metallic = 1;
            break;
    }
}

void getMaterial(in vec3 position, out vec3 normal, out vec3 albedo, out float roughness, out float metallic, out float ambientOcclusion)
{
    Scene scene;
    getScene(position, scene);
    float closest = getMinimumDistance(scene);
    if (closest == scene.mClosestObjectDistance)
    {
        getObjectMaterial(scene.mClosestObjectId, position, normal, albedo, roughness, metallic);
    }
    else
    {
        normal = getNormal(position);
        if (closest == scene.mCylinderDistance)
        {
            float z = surroundingDisplacement.z + position.z;
            albedo = 0.5 * vec3(sin(position.x * 25 + z * 23), 1, sin(position.y * 25 + z * 13));
            roughness = 1;
            metallic = 0; 
        }
        else if (closest == scene.mClosestVertexDistance)
        {
            albedo = vec3(0, 0, 0.5);
            roughness = 0.2;
            metallic = 0;
        }
        else // if (closest == scene.mPlayerDistance)
            getPlayerMaterial(position, closest, albedo, roughness, metallic);
    }
    ambientOcclusion = getAmbientOcclusion(position, normal);
}

void getRay(in vec2 uv, out vec3 origin, out vec3 direction)
    // TODO: maybe possible to get this passed from vertex shader directly?
{
    direction = normalize(eyeRotation * vec3(uv, 1.0));
    origin = eyePosition + direction * 0.2;
}

float traceDistance(in float noOfSteps, in vec3 origin, in vec3 direction)
{
    float t = 0.0;
    float extraMultiplier = 0.5;
    float extraStep = 0;
    for (int i = 0; i < noOfSteps; ++i)
    {
        float distance = getDistance(origin + direction * (t + extraStep));
        if (distance < extraStep)
        {
            if (extraStep < 0.001)
            {
                t += distance + extraStep;
                break;
            }
            extraMultiplier *= 0.5;
            extraStep *= 0.5;
            continue;
        }

        t += distance + extraStep;
        extraStep = distance * extraMultiplier;
        extraMultiplier = min(extraMultiplier * 1.25, 0.5);

        if (distance < epsilon || t > maxDistance)
            break;
    }

    return t;
}

vec3 lit(
    in vec3 worldPosition,
    in vec3 eyePosition,
    in vec3 albedo,
    in vec3 normal,
    in float roughness,
    in float metallic,
    in float ambientOcclusion
)
{
    // TODO: use some proper PBR technique
    // This temp code will do for now...
    vec3 lightDirection = vec3(0.0, 0.0, -1.0);
    float light = clamp(dot(lightDirection, normal), 0.0, 1.0) + 0.2;
    return albedo * ambientOcclusion * light;
}

void main()
{
    vec3 origin;
    vec3 direction;
    getRay(screenXY, origin, direction);

    float distance = traceDistance(65, origin, direction);
    vec3 worldPosition = origin + direction * distance;
    vec3 normal;
    vec3 albedo;
    float roughness;
    float metallic;
    float ambientOcclusion;
    getMaterial(worldPosition, normal, albedo, roughness, metallic, ambientOcclusion);
    vec3 litColor = lit(worldPosition, origin, albedo, normal, roughness, metallic, ambientOcclusion);

    // If low enough roughness we add an extra trace for reflections. TODO: refactor this to allow for arbitrary amount of reflections
    if (roughness < 0.5)
    {
        vec3 newDirection = direction - 2 * dot(direction, normal) * normal;
        vec3 newOrigin = origin + direction * (distance - epsilon * 10);
        vec3 newWorldPosition = newOrigin + newDirection * traceDistance(40, newOrigin, newDirection);
        float newRoughness;
        getMaterial(newWorldPosition, normal, albedo, newRoughness, metallic, ambientOcclusion);
        vec3 newLitColor = lit(newWorldPosition, origin, albedo, normal, newRoughness, metallic, ambientOcclusion);
        litColor = mix(newLitColor, litColor, 2 * roughness);
    }

    float fogAmount = min(16.0 / (1.0 + 0.5 * distance * distance), 1.0);
	gl_FragColor = vec4(mix(vec3(0.1, 0.07, 0.03), litColor, fogAmount), 1.0);
}
