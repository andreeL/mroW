#version 430

varying vec2 screenUV;
varying vec2 screenXY;
uniform float fTime = 0;
uniform vec2 fMouse = ivec2(0, 0);
uniform int gPoints = 0;

uniform sampler2D sceneTexture;
uniform sampler2D fontTexture;

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

vec4 blendOnto(in vec4 a, in vec4 b)
{
    return mix(a, b, b.w);
}

vec4 getCharColor(vec4 charInfo)
{
    const float signedDistance = getDistanceToChar(charInfo);
    
    const float innerCenter = 0.65;
    const float edgeCenter = 0.85;
    const float extent = 0.1;
    const float edgeColorAlpha = smoothstep(edgeCenter + extent, edgeCenter - extent, signedDistance);
    const float innerColorAlpha = smoothstep(innerCenter + extent, innerCenter - extent, signedDistance);
    const vec4 color = blendOnto(
        vec4(0.3, 0.0, 0.01, 1),
        vec4(0.8, 0.7, 0.1, innerColorAlpha)
        );
    return vec4(color.rgb, clamp(edgeColorAlpha, 0, 1));
}

vec4 getNumberChar(vec4 numberCharInfo)
{
    return getCharColor(vec4(numberCharInfo.xyz, numberCharInfo.w + 17));
}

vec4 getTestText()
{
    if (screenUV.x > 0.20 || screenUV.y < 0.85 || screenUV.y > 0.97)
        return vec4(0);

    const float size = 0.1;
    vec2 position = vec2(0.005, 0.85);
   
    const vec4 text[4] = vec4[4](
        vec4(position + vec2(0, 0.15) * size, size, 53), // T
        vec4(position + vec2(0.35, 0) * size, size, 38), // E
        vec4(position + vec2(0.75, 0.05) * size, size, 52), // S
        vec4(position + vec2(1.05, 0.1) * size, size, 53)  // T
    );

    return blendOnto(
        blendOnto(
            getCharColor(text[0]),
            getCharColor(text[1])
        ),
        blendOnto(
            getCharColor(text[2]),
            getCharColor(text[3])
        )
    );
}

vec4 getNumberText(vec4 numberCharInfo)
{
    if (screenUV.x < 0.5 || screenUV.y < 0.8 || screenUV.y > 0.95)
        return vec4(0);

    vec2 position = numberCharInfo.xy;
    float size = numberCharInfo.z;
    float number = numberCharInfo.w;
    vec4 textColor = vec4(0);
    for (int i = 0; i < 10; ++i)
    {
        number = number / 10;
        float numberChar = floor(fract(number) * 10);
        textColor = blendOnto(textColor, getNumberChar(vec4(position, size, numberChar)));
        if (number < 1)
            break;
        position -= vec2(size * 0.5, 0);
    }
    return textColor;
}

void main()
{
    vec4 sceneColor = texture(sceneTexture, screenUV);
    vec4 testFontColor = blendOnto(
        getTestText(),
        getNumberText(vec4(0.85, 0.8, 0.15, float(gPoints)))
    );
    vec4 finalColor = blendOnto(sceneColor, testFontColor);
    gl_FragColor = pow(finalColor, vec4(1.0/2.2)); // gamma corrected
}