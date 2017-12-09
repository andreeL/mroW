#version 450

varying vec2 screenUV;
varying vec2 screenXY;
uniform float fTime = 0;
uniform vec2 fMouse = ivec2(0, 0);

float epsilon = 0.005;
float maxDistance = 20.f; // TODO: set this to something sensible

vec4 takeClosest(in vec4 distance1, in vec4 distance2)
{
    return (distance1.w < distance2.w) ? distance1 : distance2;
}

vec4 getWorldAt(in vec3 position)
{
    float sphereRadius = 0.125 + 0.1 * sin(fTime * 3.1);
    vec3 qubeDisplacement = vec3(0.5, 0, fTime);
    vec3 qubeHalfDistance = vec3(0.125, 0.025, 0.125) * 2;

    vec3 closestCubePosition = fract(position + qubeDisplacement) - 0.5;
    vec4 cubeDistance = vec4(0, 0, 1, length(max(abs(closestCubePosition) - qubeHalfDistance,0.0))); // make always full lit

    vec3 closestSpherePosition = fract(position) - 0.5;
    vec4 sphereDistance = vec4(1, 0, 0, length(closestSpherePosition) - sphereRadius);

    return takeClosest(cubeDistance, sphereDistance);
}

void getRay(in vec2 uv, out vec3 origin, out vec3 direction)
{
    // TODO: maybe possible to get this passed from vertex shader directly?
    float angleX = -fMouse.x / 100.0;
    float angleY = fMouse.y / 100.0;

    vec3 target = vec3(0.5);
	origin = vec3(-sin(-angleX)* cos(-angleY), sin(-angleY), -cos(-angleX) * cos(-angleY)) * 0.50 + target;
    vec3 zAxis = normalize(target - origin);
	vec3 xAxis = normalize(cross(vec3(0, 1, 0), zAxis));
	vec3 yAxis = cross(zAxis, xAxis);

    direction = mat3(xAxis, yAxis, zAxis) * normalize(vec3(uv, 1.0));
}

vec3 getNormal(in vec3 position)
{
    // TODO: how many sample points do we want to use?
    // TODO: maybe try sample randomly
    // For now we just keep it simple and pick one sample in each axis direction (+x, -x, +y, -y, +z, -z)
    vec2 epsilon = vec2(0.0, epsilon);
    return normalize(vec3(
        getWorldAt(position + epsilon.yxx).w - getWorldAt(position - epsilon.yxx).w,
        getWorldAt(position + epsilon.xyx).w - getWorldAt(position - epsilon.xyx).w,
        getWorldAt(position + epsilon.xxy).w - getWorldAt(position - epsilon.xxy).w
    ));
}

float getAmbientOcclusion(in vec3 position, in vec3 normal)
{
    return 1; // TODO...
}

void trace(in vec3 origin, in vec3 direction, out float distance, out vec3 worldPosition, out vec3 normal, out vec3 albedo, out float roughness, out float metallic, out float ambientOcclusion)
{
    vec4 worldInfo;

    float t = 0.0;
    float extraMultiplier = 0.5;
    float extraStep = 0;
    for (int i = 0; i < 75; ++i)
    {
        worldInfo = getWorldAt(origin + direction * (t + extraStep));
        if (worldInfo.w < extraStep)
        {
            if (extraStep < 0.001)
            {
                t += worldInfo.w + extraStep;
                break;
            }
            extraMultiplier *= 0.5;
            extraStep *= 0.5;
            continue;
        }

        t += worldInfo.w + extraStep;
        extraStep = worldInfo.w * extraMultiplier;
        extraMultiplier = min(extraMultiplier * 1.25, 0.5);

        if (worldInfo.w < epsilon || t > maxDistance)
            break;
    }

    distance = t;
    worldPosition = origin + direction * t;
    normal = getNormal(worldPosition);
    albedo = worldInfo.rgb;
    roughness = 1; // we will add these later when lit impl them
    metallic = 0; // we will add these later when lit impl them
    ambientOcclusion = getAmbientOcclusion(worldPosition, normal);
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
   
    vec3 worldPosition;
   	vec3 normal;
    vec3 albedo;
    float roughness;
    float metallic;
    float ambientOcclusion;
    float distance;
    trace(origin, direction, distance, worldPosition, normal, albedo, roughness, metallic, ambientOcclusion);

    float fogAmount = min(4.0 / (1.0 + 0.5 * distance * distance), 1.0);
    vec3 litColor = lit(worldPosition, origin, albedo, normal, roughness, metallic, ambientOcclusion);
	gl_FragColor = vec4(mix(vec3(0.3, 0.2, 0.1), litColor, fogAmount), 1.0);
}
