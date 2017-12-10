#version 450

varying vec2 screenUV;
varying vec2 screenXY;
uniform float fTime = 0;
uniform vec2 fMouse = ivec2(0, 0);

float epsilon = 0.005;
float maxDistance = 30.f; // TODO: set this to something sensible

float sphereRadius = 0.125 + 0.1 * sin(fTime * 3.1);
vec3 qubeDisplacement = vec3(0.5, 0, fTime);
vec3 qubeHalfDistance = vec3(0.125, 0.025, 0.125) * 2;

struct Scene
{
    vec3 closestCubePosition;
    float mCubeDistance;
    vec3 closestSpherePosition;
    float mSphereDistance;
    float mCylinderDistance;
};

void getScene(in vec3 position, out Scene scene)
{
    scene.closestCubePosition = fract(position + qubeDisplacement) - 0.5;
    scene.mCubeDistance = length(max(abs(scene.closestCubePosition) - qubeHalfDistance,0.0));

    scene.closestSpherePosition = fract(position) - 0.5;
    scene.mSphereDistance = length(scene.closestSpherePosition) - sphereRadius;

    float cylinderExtent = 2.0;
    scene.mCylinderDistance = cylinderExtent - length(position.xy);
}

float getMinimumDistance(in Scene scene)
{
    return min(
        min(scene.mCubeDistance, scene.mSphereDistance),
        scene.mCylinderDistance
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

void getMaterial(in vec3 position, out vec3 normal, out vec3 albedo, out float roughness, out float metallic, out float ambientOcclusion)
{
    Scene scene;
    getScene(position, scene);
    float closest = getMinimumDistance(scene);
    if (closest == scene.mSphereDistance)
    {
        normal = normalize(scene.closestSpherePosition);
        albedo = vec3(1, 0, 0);
        roughness = 0.25;
        metallic = 1;
    } else
    {
        normal = getNormal(position);
        if (closest == scene.mCubeDistance)
        {
            albedo = vec3(0, 0, 1);
            roughness = 0.4;
            metallic = 0;
        } else
        {
            float z = qubeDisplacement.z + position.z;
            albedo = 0.5 * vec3(sin(position.x * 25 + z * 23), 1, sin(position.y * 25 + z * 13));
            roughness = 1;
            metallic = 0; 
        }
    }
    ambientOcclusion = getAmbientOcclusion(position, normal);
}

void getRay(in vec2 uv, out vec3 origin, out vec3 direction)
    // TODO: maybe possible to get this passed from vertex shader directly?
{
    float angleX = -fMouse.x / 100.0;
    float angleY = fMouse.y / 100.0;

    vec3 target = vec3(0.0);
	origin = vec3(-sin(-angleX)* cos(-angleY), sin(-angleY), -cos(-angleX) * cos(-angleY)) * 0.50 + target;
    vec3 zAxis = normalize(target - origin);
	vec3 xAxis = normalize(cross(vec3(0, 1, 0), zAxis));
	vec3 yAxis = cross(zAxis, xAxis);

    direction = mat3(xAxis, yAxis, zAxis) * normalize(vec3(uv, 1.0));
}

float traceDistance(in vec3 origin, in vec3 direction)
{
    float t = 0.0;
    float extraMultiplier = 0.5;
    float extraStep = 0;
    for (int i = 0; i < 100; ++i)
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

    float distance = traceDistance(origin, direction);
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
        vec3 newWorldPosition = newOrigin + newDirection * traceDistance(newOrigin, newDirection);
        float newRoughness;
        getMaterial(newWorldPosition, normal, albedo, newRoughness, metallic, ambientOcclusion);
        vec3 newLitColor = lit(newWorldPosition, origin, albedo, normal, newRoughness, metallic, ambientOcclusion);
        litColor = mix(newLitColor, litColor, 2 * roughness);
    }

    float fogAmount = min(16.0 / (1.0 + 0.5 * distance * distance), 1.0);
	gl_FragColor = vec4(mix(vec3(0.1, 0.07, 0.03), litColor, fogAmount), 1.0);
}
