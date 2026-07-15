// CRT Stage 2 - CRT display / phosphor side (Ghostty port).
// Ported from crt_display.gdshader (Godot canvas_item shader) of TurnTV.TSUISHI.
//
// Input: iChannel0 = output of crt_signal.glsl (Stage 1). Applies scanlines,
// luminance-dependent beam width, three phosphor mask styles, screen curve,
// rounded corners, vignette, bezel darkening and optional 480i field display.
//
// Load AFTER the signal stage in your Ghostty config:
//   custom-shader = shaders/crt_signal.glsl
//   custom-shader = shaders/crt_display.glsl
//   custom-shader-animation = true

// ---- Virtual source resolution (scanline count) -------------------------
// Must conceptually match the Stage 1 canvas: SOURCE_SIZE.y is the number of
// visible scanlines. iChannel0 itself is full resolution; this only sets the
// scanline pitch and low-res sample spacing.
// const vec2 SOURCE_SIZE = vec2(320.0, 240.0);
const vec2 SOURCE_SIZE = vec2(3024.0, 1964.0);

// ---- Tunable parameters (were Godot uniforms) ---------------------------
#define DISPLAY_AMOUNT           1.0    // 0 = pass Stage 1 through, 1 = full CRT display
#define SCANLINE_STRENGTH        0.42   // scanline darkening amount
#define SCANLINE_WIDTH_DARK      0.14   // beam width for dark pixels (thin => hard black)
#define SCANLINE_WIDTH_BRIGHT    0.46   // beam width for bright pixels (thick => bloom)
#define MASK_TYPE                0      // 0=off 1=slot 2=aperture grille 3=shadow mask
#define MASK_STRENGTH            0.45   // phosphor mask apply amount
#define MASK_PITCH_PX            3.5    // phosphor pitch [output px]
#define MASK_DARK                0.45   // non-active subpixel darkness
#define MASK_SOFTNESS            0.35   // mask edge softening (0 => crisp, no fwidth)
#define BRIGHTNESS_COMPENSATION  1.22   // brightness gain to offset scanline/mask loss
#define GAMMA_IN                 2.2    // input gamma (lift to linear)
#define GAMMA_OUT                2.2    // output gamma (back to display RGB)
#define CURVE_AMOUNT             0.017  // barrel distortion amount (0 = flat)
#define CORNER_RADIUS            0.035  // rounded-corner radius [UV] (0 = square)
#define VIGNETTE_STRENGTH        0.18   // edge darkening amount
#define BEZEL_STRENGTH           0.35   // push outside/corner regions toward black
#define INTERLACE_ENABLED        false  // 480i-style field display
#define INTERLACE_DIM            0.77   // how much to dim the non-active field
#define INTERLACE_BOB_PX         0.4    // field vertical bob [source px]
#define HORIZONTAL_SHARPNESS     0.15   // light horizontal unsharp from +/-1 source texel
#define CONVERGENCE_X_PX         0.25   // horizontal R/B convergence error [output px]
#define CONVERGENCE_Y_PX         0.0    // vertical R/B convergence error [output px]
#define HALATION_STRENGTH        0.12   // phosphor halation (light bleed) strength
#define HALATION_RADIUS_PX       1.5    // halation sample distance [output px]
#define HALATION_THRESHOLD       0.65   // luminance threshold for halation

// ---- Constants ----------------------------------------------------------
const vec3 CRT_STAGE2_LUMA = vec3(0.299, 0.587, 0.114);

// ---- Helpers ------------------------------------------------------------
// sRGB-like input -> linear. Clamp first to avoid negatives near zero.
vec3 crt_to_linear(vec3 color) {
    return pow(clamp(color, vec3(0.0), vec3(1.0)), vec3(GAMMA_IN));
}

// Linear -> display RGB.
vec3 crt_from_linear(vec3 color) {
    return pow(clamp(color, vec3(0.0), vec3(1.0)), vec3(1.0 / max(float(GAMMA_OUT), 0.0001)));
}

// Mask edge softening. At MASK_SOFTNESS=0 use a crisp step without fwidth.
float crt_soft_step(float edge, float value) {
    if (MASK_SOFTNESS <= 0.0001) {
        return step(edge, value);
    }
    float width = max(fwidth(value) * MASK_SOFTNESS, 0.0001);
    return smoothstep(edge - width, edge + width, value);
}

// Slot mask TV: RGB subpixels shifted half a pitch every 2 rows, vertical slots.
vec3 crt_slot_mask(vec2 screen_px) {
    float pitch = max(float(MASK_PITCH_PX), 0.0001);
    vec2 cell = floor(screen_px / pitch);
    float row_shift = mod(cell.y, 2.0) * 1.5;
    float channel = mod(floor(screen_px.x / pitch + row_shift), 3.0);
    vec3 phosphor = vec3(MASK_DARK);
    if (channel < 1.0) {
        phosphor.r = 1.0;
    } else if (channel < 2.0) {
        phosphor.g = 1.0;
    } else {
        phosphor.b = 1.0;
    }
    float slot_phase = fract(screen_px.y / pitch);
    float slot_gate = mix(1.0, float(MASK_DARK), crt_soft_step(0.55, slot_phase));
    return mix(vec3(1.0), phosphor * slot_gate, float(MASK_STRENGTH));
}

// Aperture grille / Trinitron: vertical RGB stripes, color changes horizontally.
vec3 crt_aperture_mask(vec2 screen_px) {
    float pitch = max(float(MASK_PITCH_PX), 0.0001);
    float stripe = mod(floor(screen_px.x / pitch), 3.0);
    vec3 phosphor = vec3(MASK_DARK);
    if (stripe < 1.0) {
        phosphor.r = 1.0;
    } else if (stripe < 2.0) {
        phosphor.g = 1.0;
    } else {
        phosphor.b = 1.0;
    }
    return mix(vec3(1.0), phosphor, float(MASK_STRENGTH));
}

// Shadow mask / delta triad: RGB order shifts every 2 rows for a grainy look.
vec3 crt_shadow_mask(vec2 screen_px) {
    float pitch = max(float(MASK_PITCH_PX), 0.0001);
    vec2 cell = floor(screen_px / pitch);
    float row = mod(cell.y, 2.0);
    float channel = mod(cell.x + row * 1.5, 3.0);
    vec3 triad_a = vec3(MASK_DARK);
    vec3 triad_b = vec3(MASK_DARK);
    if (channel < 1.0) {
        triad_a.r = 1.0;
        triad_b.g = 1.0;
    } else if (channel < 2.0) {
        triad_a.g = 1.0;
        triad_b.b = 1.0;
    } else {
        triad_a.b = 1.0;
        triad_b.r = 1.0;
    }
    return mix(vec3(1.0), mix(triad_a, triad_b, row), float(MASK_STRENGTH));
}

// Selected phosphor mask. MASK_TYPE=0 returns white (mask disabled).
vec3 crt_mask(vec2 screen_px) {
    if (MASK_TYPE == 1) {
        return crt_slot_mask(screen_px);
    }
    if (MASK_TYPE == 2) {
        return crt_aperture_mask(screen_px);
    }
    if (MASK_TYPE == 3) {
        return crt_shadow_mask(screen_px);
    }
    return vec3(1.0);
}

// Rounded-corner mask: 1 inside, smoothly falling to 0 past the corner.
float crt_corner_alpha(vec2 uv) {
    if (CORNER_RADIUS <= 0.0001) {
        return 1.0;
    }
    vec2 corner_uv = min(uv, 1.0 - uv);
    float min_corner = min(corner_uv.x, corner_uv.y);
    float edge = float(CORNER_RADIUS) * 0.35;
    return smoothstep(0.0, max(edge, 0.0001), min_corner);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy;
    vec2 output_size = iResolution.xy;
    float frame_count = float(iFrame);

    // Screen curve: map output UV back to a warped source UV (barrel distortion).
    vec2 centered = uv * 2.0 - 1.0;
    vec2 warped = centered * (1.0 + CURVE_AMOUNT * centered.yx * centered.yx);
    vec2 source_uv = warped * 0.5 + 0.5;
    vec3 plain_rgb = texture(iChannel0, clamp(uv, vec2(0.0), vec2(1.0))).rgb;

    // Pixels whose warped UV falls outside the screen become bezel (black).
    float inside = step(0.0, source_uv.x) * step(source_uv.x, 1.0) * step(0.0, source_uv.y) * step(source_uv.y, 1.0);
    vec2 sample_uv = clamp(source_uv, vec2(0.0), vec2(1.0));

    // 480i field bob: nudge sample position half a pixel per field.
    float field = mod(frame_count, 2.0);
    if (INTERLACE_ENABLED) {
        sample_uv.y += (field - 0.5) * INTERLACE_BOB_PX / max(SOURCE_SIZE.y, 1.0);
        sample_uv = clamp(sample_uv, vec2(0.0), vec2(1.0));
    }

    // Low-res source sampling with R/B convergence error (output-pixel based).
    vec3 source_rgb = texture(iChannel0, sample_uv).rgb;
    vec2 convergence_uv = vec2(CONVERGENCE_X_PX, CONVERGENCE_Y_PX) / max(output_size, vec2(1.0));
    if (abs(float(CONVERGENCE_X_PX)) > 0.0001 || abs(float(CONVERGENCE_Y_PX)) > 0.0001) {
        source_rgb.r = texture(iChannel0, clamp(sample_uv + convergence_uv, vec2(0.0), vec2(1.0))).r;
        source_rgb.b = texture(iChannel0, clamp(sample_uv - convergence_uv, vec2(0.0), vec2(1.0))).b;
    }

    // Light horizontal unsharp from +/-1 source texel (pre-blur sharpen).
    if (HORIZONTAL_SHARPNESS > 0.0001) {
        vec2 source_texel_x = vec2(1.0 / max(SOURCE_SIZE.x, 1.0), 0.0);
        vec3 horizontal_blur = (
            texture(iChannel0, clamp(sample_uv - source_texel_x, vec2(0.0), vec2(1.0))).rgb
            + texture(iChannel0, clamp(sample_uv + source_texel_x, vec2(0.0), vec2(1.0))).rgb) * 0.5;
        source_rgb = clamp(source_rgb + (source_rgb - horizontal_blur) * HORIZONTAL_SHARPNESS, vec3(0.0), vec3(1.0));
    }
    vec3 linear_rgb = crt_to_linear(source_rgb);

    // Halation: gather bright neighbors now, add back after the mask.
    vec3 halation_linear = vec3(0.0);
    if (HALATION_STRENGTH > 0.0001) {
        vec2 halo_uv = vec2(HALATION_RADIUS_PX) / max(output_size, vec2(1.0));
        vec3 halo_rgb = (
            texture(iChannel0, clamp(sample_uv + vec2(halo_uv.x, 0.0), vec2(0.0), vec2(1.0))).rgb
            + texture(iChannel0, clamp(sample_uv - vec2(halo_uv.x, 0.0), vec2(0.0), vec2(1.0))).rgb
            + texture(iChannel0, clamp(sample_uv + vec2(0.0, halo_uv.y), vec2(0.0), vec2(1.0))).rgb
            + texture(iChannel0, clamp(sample_uv - vec2(0.0, halo_uv.y), vec2(0.0), vec2(1.0))).rgb) * 0.25;
        float halo_gate = smoothstep(min(float(HALATION_THRESHOLD), 0.9999), 1.0, dot(halo_rgb, CRT_STAGE2_LUMA));
        halation_linear = crt_to_linear(halo_rgb) * halo_gate;
    }

    // Scanlines: distance from source line center, luma-dependent beam width.
    float src_y = sample_uv.y * SOURCE_SIZE.y;
    float line_dist = abs(fract(src_y) - 0.5);
    float luma = dot(source_rgb, CRT_STAGE2_LUMA);
    float beam_width = mix(float(SCANLINE_WIDTH_DARK), float(SCANLINE_WIDTH_BRIGHT), luma);
    float scan = exp(-(line_dist * line_dist) / max(beam_width, 0.0001));
    float scan_gain = mix(1.0 - SCANLINE_STRENGTH, 1.0, scan);
    linear_rgb *= scan_gain;

    // 480i: dim source lines that do not match the current field.
    if (INTERLACE_ENABLED) {
        float src_line = floor(sample_uv.y * SOURCE_SIZE.y);
        float field_match = 1.0 - abs(mod(src_line, 2.0) - field);
        linear_rgb *= mix(float(INTERLACE_DIM), 1.0, field_match);
    }

    // Phosphor mask, computed in final output-pixel space.
    vec2 screen_px = uv * output_size;
    linear_rgb *= crt_mask(screen_px);

    // Brightness compensation, vignette, rounded corners and bezel.
    linear_rgb += halation_linear * HALATION_STRENGTH;
    linear_rgb *= BRIGHTNESS_COMPENSATION;
    float edge_dist = min(min(source_uv.x, 1.0 - source_uv.x), min(source_uv.y, 1.0 - source_uv.y));
    float vignette = mix(1.0, smoothstep(0.0, 0.35, edge_dist), VIGNETTE_STRENGTH);
    float corner = crt_corner_alpha(source_uv);
    float screen_alpha = inside * corner;
    linear_rgb *= vignette * screen_alpha;

    vec3 crt_rgb = crt_from_linear(linear_rgb);
    vec3 bezel_rgb = vec3(0.0);
    crt_rgb = mix(bezel_rgb, crt_rgb, mix(screen_alpha, 1.0, 1.0 - BEZEL_STRENGTH));

    // DISPLAY_AMOUNT=0 shows Stage 1 as-is, 1 applies the full CRT display.
    fragColor = vec4(mix(plain_rgb, crt_rgb, clamp(float(DISPLAY_AMOUNT), 0.0, 1.0)), 1.0);
}
