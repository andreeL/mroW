#version 450

varying vec2 screenUV;
varying vec2 screenXY;
uniform float fTime = 0;
uniform vec2 fMouse = ivec2(0, 0);

void getRay(in vec2 uv, out vec3 start, out vec3 direction)
{
    float angleX = -fMouse.x / 100.0;
    float angleY = fMouse.y / 100.0;

    vec3 target = vec3(0.5);
	start = vec3(-sin(-angleX)* cos(-angleY), sin(-angleY), -cos(-angleX) * cos(-angleY)) * 0.50 + target;
    vec3 zAxis = normalize(target - start);
	vec3 xAxis = normalize(cross(vec3(0, 1, 0), zAxis));
	vec3 yAxis = cross(zAxis, xAxis);

    direction = mat3(xAxis, yAxis, zAxis) * normalize(vec3(uv, 1.0));
}

void main()
{
    vec3 start;
    vec3 direction;
    getRay(screenXY, start, direction);
   
    float sphereRadius = 0.125 + 0.1 * sin(fTime * 3.1);
    vec3 qubeDisplacement = vec3(0.5, 0, fTime);
    vec3 qubeHalfDistance = vec3(0.125, 0.025, 0.125);
    vec4 dist;

    float t = 0.0;
    for (int i = 0; i < 100; ++i)
    {
        vec3 point = start + direction * t;

        // distance/normal function
        vec3 closestCubePosition = fract(point + qubeDisplacement) - 0.5;
        vec4 cubeDistance = vec4(0, 0, -2, length(max(abs(closestCubePosition) - qubeHalfDistance,0.0))); // make always full lit

        vec3 closestSpherePosition = fract(point) - 0.5;
        vec4 sphereDistance = vec4(normalize(closestSpherePosition), length(closestSpherePosition) - sphereRadius);

        dist = (cubeDistance.w < sphereDistance.w) ? cubeDistance : sphereDistance;

        t += dist.w * 1.0;
        if (dist.w < 0.005)
            break;
    }
    dist = vec4(dist.xyz, t);
    
    float fog = min(4.0 / (1.0 + 0.5 * dist.w * dist.w), 1.0);
    
    vec3 lightDirection = vec3(0.0, 0.0, -1.0);
    vec3 materialColor = floor(0.5 + length(dist.xyz)) == 2.0 ? vec3(0.3, 0.59, 0.72) : vec3(0.72, 0.59, 0.3);
    
    float light = clamp(dot(lightDirection, dist.xyz), 0.0, 1.0) + 0.2;

	gl_FragColor = vec4(materialColor * (light * fog),1.0);
}
