// Ghostty custom shader (Shadertoy-compatible)
// Pure Vector CRT look: bright core + outward glow + subtle flicker.
// No cursor logic, no time-based persistence. Designed to be composable.
//
// Usage: set as one of your custom-shader entries in Ghostty.
// Input: iChannel0 = terminal framebuffer.
// Output: vectorized "beam" appearance by treating high-contrast edges as strokes.

//// CONFIGURATION ////

// Overall intensity
const float CORE_GAIN   = 0.40;   // sharp beam core brightness
const float GLOW_GAIN   = 0.20;   // glow brightness
const float BLACK_CRUSH = 0.01;   // pushes near-black toward black (0..0.12)

// Stroke detection (edge-based)
const float EDGE_THRESH = 0.20;   // smaller => more strokes
const float EDGE_SOFT   = 1.20;   // softness around threshold

// Radii in pixels (scale with resolution)
const float CORE_RADIUS_PX = 0.2;
const float GLOW_RADIUS_PX = 1.0;

// Flicker / noise (keep subtle)
const float FLICKER_AMPL = 0.0; // scan instability feel
const float NOISE_AMPL   = 0.50;

// Tint: set to vec3(1.0) for "no tint", or classic vector green/cyan
//const vec3 VECTOR_TINT = vec3(1.00);
const vec3 VECTOR_TINT = vec3(0.55, 1.00, 0.78);

//// UTILS ////

float luma(vec3 c) { return dot(c, vec3(0.2126, 0.7152, 0.0722)); }

float hash12(vec2 p) {
    vec3 p3 = fract(vec3(p.xyx) * 0.1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

vec3 tex0(vec2 uv) { return texture(iChannel0, uv).rgb; }

float edgeMag(vec2 uv, vec2 duv) {
    // Cheap Sobel-like gradient magnitude on luminance
    float c  = luma(tex0(uv));
    float l  = luma(tex0(uv + vec2(-duv.x, 0.0)));
    float r  = luma(tex0(uv + vec2( duv.x, 0.0)));
    float u  = luma(tex0(uv + vec2(0.0,  duv.y)));
    float d  = luma(tex0(uv + vec2(0.0, -duv.y)));
    float gx = r - l;
    float gy = u - d;
    return sqrt(gx*gx + gy*gy);
}

float strokeMask(vec2 uv, vec2 duv) {
    float e = edgeMag(uv, duv);
    return smoothstep(EDGE_THRESH, EDGE_THRESH + EDGE_SOFT, e);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv  = fragCoord.xy / iResolution.xy;
    vec2 duv = 1.0 / iResolution.xy;

    // Base frame
    vec3 base = tex0(uv);
    base = max(base - BLACK_CRUSH, 0.0);

    // Primary stroke detection
    float s = strokeMask(uv, duv);

    // Beam core: reinforce strokes by sampling a tight neighborhood.
    // This gives a sharper "bright core" like a focused electron beam.
    float core = 0.0;
    {
        vec2 o = duv * CORE_RADIUS_PX;
        core += 2.2 * s;
        core += 1.0 * strokeMask(uv + vec2( o.x, 0.0), duv);
        core += 1.0 * strokeMask(uv + vec2(-o.x, 0.0), duv);
        core += 1.0 * strokeMask(uv + vec2(0.0,  o.y), duv);
        core += 1.0 * strokeMask(uv + vec2(0.0, -o.y), duv);
        core += 0.8 * strokeMask(uv + vec2( o.x,  o.y), duv);
        core += 0.8 * strokeMask(uv + vec2(-o.x,  o.y), duv);
        core += 0.8 * strokeMask(uv + vec2( o.x, -o.y), duv);
        core += 0.8 * strokeMask(uv + vec2(-o.x, -o.y), duv);
        core /= 9.2;
        core = clamp(core, 0.0, 1.0);
        core = pow(core, 0.75); // brighten the center
    }

    // Glow: wider neighborhood blur of the stroke field (multi-tap).
    // Tuned to look like outward phosphor bloom, not Gaussian blur haze.
    float glow = 0.0;
    {
        vec2 o1 = duv * (GLOW_RADIUS_PX * 1.0);
        vec2 o2 = duv * (GLOW_RADIUS_PX * 0.65);
        vec2 o3 = duv * (GLOW_RADIUS_PX * 0.35);

        // Center
        glow += 1.00 * s;

        // Cardinal directions
        glow += 0.85 * strokeMask(uv + vec2( o1.x, 0.0), duv);
        glow += 0.85 * strokeMask(uv + vec2(-o1.x, 0.0), duv);
        glow += 0.85 * strokeMask(uv + vec2(0.0,  o1.y), duv);
        glow += 0.85 * strokeMask(uv + vec2(0.0, -o1.y), duv);

        // Mid ring
        glow += 0.65 * strokeMask(uv + vec2( o2.x,  o2.y), duv);
        glow += 0.65 * strokeMask(uv + vec2(-o2.x,  o2.y), duv);
        glow += 0.65 * strokeMask(uv + vec2( o2.x, -o2.y), duv);
        glow += 0.65 * strokeMask(uv + vec2(-o2.x, -o2.y), duv);

        // Inner ring (adds density near core)
        glow += 0.55 * strokeMask(uv + vec2( o3.x, 0.0), duv);
        glow += 0.55 * strokeMask(uv + vec2(-o3.x, 0.0), duv);
        glow += 0.55 * strokeMask(uv + vec2(0.0,  o3.y), duv);
        glow += 0.55 * strokeMask(uv + vec2(0.0, -o3.y), duv);

        glow /= (1.00 + 4.0*0.85 + 4.0*0.65 + 4.0*0.55);
        glow = clamp(glow, 0.0, 1.0);
        glow = pow(glow, 1.35); // make glow fall off faster
    }

    // Subtle flicker/noise, stronger where the beam is bright.
    float n = hash12(fragCoord.xy + iTime * 60.0) - 0.5;
    float flicker = 1.0
        + n * NOISE_AMPL
        + sin(iTime * 80.0 + uv.y * 11.0) * FLICKER_AMPL;

    // Vector emission (additive)
    vec3 emit = VECTOR_TINT * (CORE_GAIN * core + GLOW_GAIN * glow) * flicker;

    // Compose: keep base but treat strokes as emissive beam.
    // If you want "pure beam only", replace `base + emit` with `emit`.
    vec3 outc = base + emit;

    // Optional gentle vignette to feel more like glass (very subtle)
    vec2 p = uv * 2.0 - 1.0;
    float vig = 1.0 - 0.10 * dot(p, p);
    outc *= clamp(vig, 0.0, 1.0);

    fragColor = vec4(clamp(outc, 0.0, 1.0), 1.0);
}
