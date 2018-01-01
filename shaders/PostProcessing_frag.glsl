#version 430

varying vec2 screenUV;
varying vec2 screenXY;
uniform float fTime = 0;
uniform vec2 fMouse = ivec2(0, 0);

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
    
    const float center = 0.45;
    const float extent = 0.1;
    const float colorAlpha = smoothstep(center + extent, center - extent, signedDistance);
    const vec4 color = vec4(0.8, 0.7, 0.1, clamp(colorAlpha, 0, 1));

    const float edgeCenter = 0.85;
    const float edgeExtent = 0.1;
    const float edgeColorAlpha = smoothstep(edgeCenter + edgeExtent, edgeCenter - edgeExtent, signedDistance);
    const vec4 edgeColor = vec4(0.3, 0.0, 0.01, clamp(edgeColorAlpha, 0, 1));

    return blendOnto(edgeColor, color);
}

vec4 getTestText()
{
    const float size = 0.1;
    const vec4 text[4] = vec4[4](
        vec4(0.005, 0.8  , size, 53), // T
        vec4(0.04 , 0.785, size, 38), // E
        vec4(0.08 , 0.79 , size, 52), // S
        vec4(0.11 , 0.795, size, 53)  // T
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

void main()
{
    vec4 sceneColor = texture(sceneTexture, screenUV);
    vec4 testFontColor = getTestText();
    vec4 finalColor = blendOnto(sceneColor, testFontColor);
    gl_FragColor = pow(finalColor, vec4(1.0/2.2)); // gamma corrected
}