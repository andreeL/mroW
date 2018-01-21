#version 430

varying vec2 screenUV;
varying vec2 screenXY;
uniform float fTime = 0;
uniform int gPoints = 0;
uniform int gCurrentMenuOption = -1;

uniform sampler2D sceneTexture;
uniform sampler2D fontTexture;

const vec4 defaultTextInnerColor = vec4(0.3, 0.0, 0.01, 1);
const vec4 defaultTextOuterColor = vec4(0.8, 0.7, 0.1, 1);
const vec4 selectedTextInnerColor = vec4(1.0, 0.0, 0.01, 1);
const vec4 selectedTextOuterColor = vec4(1.0, 0.7, 0.1, 1);

float getDistanceToChar(vec4 charInfo)
{
    const vec2 position = charInfo.xy;
    const float size = charInfo.z;
    const vec2 charIndex = vec2(16.0 * fract(charInfo.w / 16.0), floor(charInfo.w / 16.0));

    const vec2 charUV = (screenUV - position) / size;
    const float edge = 0.5/32.0; // we cut half a texel to avoid neighbouring glyphs
    if (charUV.x < edge || charUV.x > 1.0 - edge || charUV.y < edge || charUV.y > 1.0 - edge)
        return 1;
    else
        return texture2D(fontTexture, (charUV + charIndex) / 16).r;
}

float getDistanceToNumber(vec4 numberCharInfo)
{
    return getDistanceToChar(vec4(numberCharInfo.xyz, numberCharInfo.w + 48));
}

vec4 blendOnto(in vec4 a, in vec4 b)
{
    return mix(a, b, b.w);
}

vec4 getColor(in float signedDistance, in vec4 color1, in vec4 color2)
{
    const float innerCenter = 0.503;
    const float edgeCenter = 0.62;
    const float extent = 0.01;
    const float edgeColorAlpha = smoothstep(edgeCenter + extent, edgeCenter - extent, signedDistance);
    const float innerColorAlpha = smoothstep(innerCenter + extent, innerCenter - extent, signedDistance);
    const vec4 color = blendOnto(
        color1,
        vec4(color2.xyz, color2.w * innerColorAlpha)
        );
    return vec4(color.rgb, clamp(edgeColorAlpha, 0, 1));
}

vec4 getMenuTextColor(in float signedDistance, bool selected)
{
    if (selected)
        return getColor(signedDistance, selectedTextInnerColor, selectedTextOuterColor);
    else
        return getColor(signedDistance, defaultTextInnerColor, defaultTextOuterColor);
}

vec4 getNumberText(vec4 numberCharInfo)
{
    if (screenUV.x < 0.5 || screenUV.y < 0.8 || screenUV.y > 0.95)
        return vec4(0);

    vec2 position = numberCharInfo.xy;
    float size = numberCharInfo.z;
    float number = numberCharInfo.w;
    float distance = 1;
    for (int i = 0; i < 10; ++i)
    {
        number = floor(number) / 10;
        float numberChar = round(fract(number) * 10);
        distance = min(distance, getDistanceToNumber(vec4(position, size, numberChar)));
        if (number < 1)
            break;
        position -= vec2(size * 0.5, 0);
    }
    return getMenuTextColor(distance, false);
}

vec4 getMenuStartText()
{
    const vec2 position = vec2(0.5, 0.6);
    const float size = 0.0025;
    const float size2 = 0.085;
    const vec2 downLeft = vec2(-130.0,-8) * size + position;
    const vec2 upRight = vec2(130.0,24) * size + position;
    if (screenUV.x < downLeft.x || screenUV.x > upRight.x || screenUV.y < downLeft.y || screenUV.y > upRight.y)
        return vec4(0.0);

    const vec4 text[19] = vec4[19]( // Enter the mroW hole
        vec4(position + vec2(-128.0,-3) * size, size2, 69), // 'E'
        vec4(position + vec2(-113.0,-4) * size, size2, 110), // 'n'
        vec4(position + vec2(-101.0,-5) * size, size2, 116), // 't'
        vec4(position + vec2(-90.0,-5) * size, size2, 101), // 'e'
        vec4(position + vec2(-73.0,-4) * size, size2, 114), // 'r'
        vec4(position + vec2(-64.0,-3) * size, size2, 32), // ' '
        vec4(position + vec2(-52.0,-5) * size, size2, 116), // 't'
        vec4(position + vec2(-41.0,-4) * size, size2, 104), // 'h'
        vec4(position + vec2(-28.0,-5) * size, size2, 101), // 'e'
        vec4(position + vec2(-12.0,-3) * size, size2, 32), // ' '
        vec4(position + vec2(0.0,-6) * size, size2, 109), // 'm'
        vec4(position + vec2(19.0,-4) * size, size2, 114), // 'r'
        vec4(position + vec2(29.0,-4) * size, size2, 111), // 'o'
        vec4(position + vec2(42.0,-5) * size, size2, 87), // 'W'
        vec4(position + vec2(62.0,-3) * size, size2, 32), // ' '
        vec4(position + vec2(74.0,-4) * size, size2, 104), // 'h'
        vec4(position + vec2(87.0,-4) * size, size2, 111), // 'o'
        vec4(position + vec2(100.0,-6) * size, size2, 108), // 'l'
        vec4(position + vec2(107.0,-5) * size, size2, 101) // 'e'
    );
    
    float distance = 1;
    for (int i = 0; i < 19; ++i)
        distance = min(distance, getDistanceToChar(text[i]));
    return getMenuTextColor(distance, gCurrentMenuOption == 0);
}

vec4 getMenuExitText()
{
    const vec2 position = vec2(0.5, 0.5);
    const float size = 0.0025;
    const float size2 = 0.085;
    const vec2 downLeft = vec2(-26.5,-8) * size + position;
    const vec2 upRight = vec2(26.5,24) * size + position;
    if (screenUV.x < downLeft.x || screenUV.x > upRight.x || screenUV.y < downLeft.y || screenUV.y > upRight.y)
        return vec4(0.0);
        
    const vec4 text[4] = vec4[4](
        vec4(position + vec2(-24.5,-3) * size, size2, 69), // 'E'
        vec4(position + vec2(-9.5,-5) * size, size2, 120), // 'x'
        vec4(position + vec2(5.5,-4) * size, size2, 105), // 'i'
        vec4(position + vec2(9.5,-5) * size, size2, 116) // 't'
    );

    float distance = 1;
    for (int i = 0; i < 4; ++i)
        distance = min(distance, getDistanceToChar(text[i]));
    return getMenuTextColor(distance, gCurrentMenuOption == 1);
}

void main()
{
    vec4 sceneColor = texture(sceneTexture, screenUV);
    vec4 pointsColor = getNumberText(vec4(0.85, 0.8, 0.15, float(gPoints)));
    vec4 menuColor = blendOnto(getMenuStartText(), getMenuExitText());
    vec4 guiColor = gCurrentMenuOption == -1 ? pointsColor : blendOnto(menuColor, pointsColor);

    vec4 finalColor = blendOnto(sceneColor, guiColor);
    gl_FragColor = pow(finalColor, vec4(1.0/2.2)); // gamma corrected
}