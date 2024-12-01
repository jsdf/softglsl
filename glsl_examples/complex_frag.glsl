#version 300 es
precision highp float;

in vec4 vColor;
in vec2 vTextureCoord;
in vec4 vTextureClippingCoords;
in vec4 vScreenSpaceRect; // bounds of the rect in screen space coords as {minX, minY, maxX, maxY}
flat in uint vHovered;

uniform vec3 uMouse; // x,y = mouse position in screen space, z = mouse button state (0.0f = up, 1.0f = down)
uniform sampler2D uTextureSampler;
uniform vec2 uViewportSize; // Viewport size in logical pixels (e.g., vec2(800, 600))
uniform float uPixelRatio; // Pixel ratio of the screen (e.g., 2.0f for retina displays) for gl_FragCoord conversion
uniform bool uYIsUp; // Whether the y-axis is up (true) or down (false)

out vec4 FragColor;

// Convert gl_FragCoord to logical coordinates
// gl_FragCoord is in screen space coordinates
// (0, 0) is the bottom left corner of the screen
// logical coordinates have (0, 0) at the top left corner of the screen
// also account for the pixel ratio
vec2 getLogicalFragCoord() {
    vec2 logicalCoord = gl_FragCoord.xy / uPixelRatio;

    if(!uYIsUp) {
        // flip y axis
        logicalCoord.y = uViewportSize.y - logicalCoord.y;
    }
    return logicalCoord;
}

// b.x = width
// b.y = height
// r.x = roundness top-right  
// r.y = roundness bottom-right
// r.z = roundness top-left
// r.w = roundness bottom-left
float sdRoundBox(in vec2 p, in vec2 b, in vec4 r) {
    r.xy = (p.x > 0.0f) ? r.xy : r.zw;
    r.x = (p.y > 0.0f) ? r.x : r.y;
    vec2 q = abs(p) - b + r.x;
    return min(max(q.x, q.y), 0.0f) + length(max(q, 0.0f)) - r.x;
}

vec4 getMouseGlowColor(
    in vec2 logicalFragCoord,
    in vec2 mousePos
) {
    vec2 p = logicalFragCoord - mousePos; // distance from the mouse

    vec2 box = vec2(1.0f, 1.0f);

    vec4 cornerRadius = vec4(5.f, 5.f, 5.f, 5.f);
    cornerRadius = min(cornerRadius, min(box.x, box.y));

    float d = sdRoundBox(p, box, cornerRadius);

    float size = 2000.f;
    float maxOpacity = 0.2f;

    float falloff = maxOpacity - clamp(d / size, 0.f, maxOpacity);
    return vec4(0.65f, 0.85f, 1.0f, falloff);
}

bool isFragInsideRect(
    in vec2 logicalCoord
) {
    return logicalCoord.x >= vScreenSpaceRect.x && logicalCoord.y >= vScreenSpaceRect.y &&
        logicalCoord.x <= vScreenSpaceRect.z && logicalCoord.y <= vScreenSpaceRect.w;
}

// use signed distance field to determine if a point is inside the rect, as a rounded rect
float roundRectSDFTest(
    in vec2 logicalFragCoord
) {
    // Compute the screen space rectangle size
    vec2 rectSize = (vScreenSpaceRect.zw - vScreenSpaceRect.xy); // TODO: previously this had * 0.5f

    vec2 rectCenter = (vScreenSpaceRect.xy + rectSize * 0.5f); // center of the rectangle = (min + half of size)
    // compute the position of the fragment relative to the center of the rectangle
    vec2 localPos = logicalFragCoord - rectCenter;

    // Define the roundness of the rectangle corners
    vec4 roundness = vec4(8.0f, 8.0f, 8.0f, 8.0f); // Example values, adjust as needed

    // Compute the distance using the SDF
    return sdRoundBox(localPos, rectSize, roundness);
}

void main(void) {
    bool clip = vTextureCoord.x < vTextureClippingCoords.x || vTextureCoord.x > vTextureClippingCoords.z ||
        vTextureCoord.y < vTextureClippingCoords.y || vTextureCoord.y > vTextureClippingCoords.w;

    bool mouseHighlightEnabled = uViewportSize.x > 1000000.0f;
    vec2 logicalFragCoord = getLogicalFragCoord();

    // ensure uMouse doesn't get optimized out
    if(uViewportSize.x > 1000000.0f) {
        FragColor = vec4(uViewportSize.xy, uMouse.xy);
    }

    float distance = roundRectSDFTest(logicalFragCoord);

    vec4 color = vColor;

    // test logicalFragCoord
    if(uViewportSize.x < 1000000.0f) {
        color = vec4(logicalFragCoord / uViewportSize, 0.f, 1.0f);
    }

    // Use the distance to determine shading
    if(distance < -2.0f) {
        // Inside the rounded rectangle
        // given a border of 2 logical pixels
        // color = vColor;
        color = vec4(-(distance / uViewportSize.x), -(distance / uViewportSize.y), 0.f, 1.0f);
    } else if(distance < 0.0f) {
        // On the border
        FragColor = vHovered == 1u ? vec4(1.f, 1.f, 1.f, 1.f) : vec4(0.0f, 0.0f, 0.0f, 1.0f);
        return; // only output border color
    } else {
        // Outside the rounded rectangle
        // discard; // TODO: enable this once SDF test is working
        color = vec4(1.f - (min(uViewportSize.x, distance) / uViewportSize.x), 0.0f, 0.0f, 1.0f);
    }

    // if(isFragInsideRect(logicalFragCoord)) {
    //     FragColor = vec4(0.08f, 0.18f, 0.79f, 1.0f);
    //     return;
    // }

    if(mouseHighlightEnabled) {
        vec4 mouseGlowColor = vHovered == 1u ? getMouseGlowColor(logicalFragCoord, uMouse.xy) : vec4(0.0f, 0.0f, 0.0f, 0.0f);

        color = mix(color, mouseGlowColor, mouseGlowColor.a);
    }

    if(clip) {
        // just output background color
        FragColor = color;
    } else {
        // sample the texture  
        vec4 texel = texture(uTextureSampler, vTextureCoord);
        // mix by the alpha value of the texture to overlay the texture on top of the color
        FragColor = mix(color, vec4(texel.rgb, 1.0f), texel.a);
    }
}