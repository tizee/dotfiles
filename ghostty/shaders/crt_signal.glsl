// CRT Stage 1 - NTSC composite signal path (Ghostty port).
// Ported from crt_signal.gdshader (Godot canvas_item shader) of TurnTV.TSUISHI.
//
// Simulates composite/RF signal degradation: chroma bleed, false color, dot
// crawl, ghosting and analog noise. Scanlines, phosphor mask, screen curve and
// other "display side" effects belong to crt_display.glsl (Stage 2).
//
// Ghostty chains custom shaders: the output of a previous shader is written into
// iChannel0 of the next one. Load this BEFORE the display stage:
//   custom-shader = shaders/crt_signal.glsl
//   custom-shader = shaders/crt_display.glsl
//   custom-shader-animation = true

// ---- Virtual NTSC canvas resolution -------------------------------------
// The original game rendered to a 320x240 SubViewport. Here iChannel0 is the
// full-resolution terminal, so we treat it as if it were this logical canvas.
// Larger values => finer artifacts and crisper text; 320x240 is the retro look.
// const vec2 SOURCE_SIZE = vec2(320.0, 240.0);
const vec2 SOURCE_SIZE = vec2(3024.0, 1964.0);

// ---- Tunable parameters (were Godot uniforms) ---------------------------
#define SIGNAL_AMOUNT        0.4    // overall amount; 0 = untouched, 1 = full degradation
#define COMPOSITE_ARTIFACT   0.85   // luma->chroma leak (cross color / false color)
#define COMPOSITE_FRINGING   0.65   // chroma->luma leak (dot fringing / smear)
#define SUBCARRIER_PHASE_PX  1.047  // subcarrier phase per source pixel [rad] (pi/3)
#define LINE_PHASE_AMOUNT    2.094  // per-line phase step [rad] (2pi/3, 3-phase)
#define PHASE_JITTER         0.025  // per-line/per-frame phase jitter
#define CHROMA_DELAY_PX      1.25   // horizontal chroma delay [px] (color shifted right)
#define Y_BAND_PX            1.25   // Y lowpass FIR radius [px] (~4.2MHz)
#define I_BAND_PX            2.75   // I lowpass FIR radius [px] (~1.3MHz)
#define Q_BAND_PX            4.25   // Q lowpass FIR radius [px] (~0.4-0.6MHz)
#define GHOST_STRENGTH       0.09   // multipath ghost intensity
#define GHOST_OFFSET_PX      6.0    // ghost reflection distance [px]
#define NOISE_LUMA           0.012  // luma snow noise
#define NOISE_CHROMA         0.018  // chroma flicker noise
#define RGB_BYPASS_MIX       0.0    // 1 => pass Stage 1 through (RGB connection)
#define RF_AMOUNT            0.20   // overall extra RF effect amount
#define SYNC_JITTER_PX       0.18   // horizontal sync PLL residual wobble [px]
#define RF_TUNING_ERROR      0.08   // RF detuning: phase tilt + drift
#define BURST_PHASE_NOISE    0.03   // color burst phase-lock residual (hue wobble)
#define RF_SNOW              0.015  // fine RF snow + white impulse noise
#define RF_GAIN_WOBBLE       0.015  // AGC residual low-frequency amplitude wobble
#define HUE_DEG              0.0    // post-demod hue trim [degrees]
#define SATURATION           1.0    // post-demod saturation (0 = mono, 1 = unity)

// ---- Constants ----------------------------------------------------------
const float CRT_TAU = 6.2831853071795865;
// Fixed FIR radius; 8 => 17 taps. Loop count must be compile-time constant.
const int CRT_FIR_RADIUS = 8;
// RGB->YIQ (columns of the matrix, since mat3 is column-major).
const mat3 CRT_RGB_TO_YIQ = mat3(
    vec3(0.2989, 0.5959, 0.2115),
    vec3(0.5870, -0.2744, -0.5229),
    vec3(0.1140, -0.3216, 0.3114));
// YIQ->RGB inverse.
const mat3 CRT_YIQ_TO_RGB = mat3(
    vec3(1.0, 1.0, 1.0),
    vec3(0.956, -0.272, -1.106),
    vec3(0.621, -0.647, 1.705));
const vec3 CRT_LUMA_WEIGHT = vec3(0.2989, 0.5870, 0.1140);

// ---- Helpers ------------------------------------------------------------
// Arithmetic 3D hash (0..1). Random source for noise and phase jitter.
float crt_hash(vec3 p) {
    p = fract(p * vec3(0.1031, 0.1030, 0.0973));
    p += dot(p, p.yzx + 33.33);
    return fract((p.x + p.y) * p.z);
}

// Gaussian window weight. Larger radius => wider blur => narrower band.
float crt_gaussian_weight(float distance_px, float radius_px) {
    float sigma = max(radius_px, 0.0001);
    return exp(-(distance_px * distance_px) / (2.0 * sigma * sigma));
}

// NTSC subcarrier phase [rad] at a given source pixel position.
float crt_subcarrier_phase(float pixel_x, float pixel_y, float frame_index) {
    float line_index = floor(pixel_y);
    float line_phase = LINE_PHASE_AMOUNT * mod(line_index, 3.0);
    float frame_phase = LINE_PHASE_AMOUNT * mod(frame_index, 2.0);
    float jitter = (crt_hash(vec3(line_index, frame_index, 17.0)) - 0.5) * PHASE_JITTER * CRT_TAU;
    float burst_error = (crt_hash(vec3(line_index, floor(frame_index * 0.25), 71.0)) - 0.5) * BURST_PHASE_NOISE * CRT_TAU;
    float tuning_drift = RF_TUNING_ERROR * ((pixel_x / max(SOURCE_SIZE.x, 1.0)) + frame_index * 0.006) * CRT_TAU;
    return pixel_x * SUBCARRIER_PHASE_PX + line_phase + frame_phase + jitter + RF_AMOUNT * (burst_error + tuning_drift);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy;
    float frame_count = float(iFrame);

    // Full bypass: skip the 17-tap FIR entirely.
    if (SIGNAL_AMOUNT <= 0.0001 || RGB_BYPASS_MIX >= 0.9999) {
        fragColor = texture(iChannel0, uv);
        return;
    }

    // Horizontal sync PLL residual: slow per-line drift + slow random.
    float source_line = floor(uv.y * SOURCE_SIZE.y);
    float sync_slow = sin(source_line * 0.093 + frame_count * 0.018);
    float sync_noise = crt_hash(vec3(source_line, floor(frame_count * 0.25), 41.0)) - 0.5;
    float sync_offset = RF_AMOUNT * SYNC_JITTER_PX * (sync_slow * 0.65 + sync_noise * 0.70) / max(SOURCE_SIZE.x, 1.0);
    vec2 signal_uv = clamp(uv + vec2(sync_offset, 0.0), vec2(0.0), vec2(1.0));
    vec2 src_px = signal_uv * SOURCE_SIZE;
    vec3 source_rgb = texture(iChannel0, uv).rgb;
    vec2 texel = vec2(1.0) / SOURCE_SIZE;

    // Edge factor from left/right luma diff; limits false color to luma edges.
    float luma_left = dot(texture(iChannel0, signal_uv - vec2(texel.x, 0.0)).rgb, CRT_LUMA_WEIGHT);
    float luma_right = dot(texture(iChannel0, signal_uv + vec2(texel.x, 0.0)).rgb, CRT_LUMA_WEIGHT);
    float edge_factor = clamp(abs(luma_right - luma_left) * 4.0, 0.0, 1.0);
    float artifact_gain = COMPOSITE_ARTIFACT * edge_factor;

    // Modulate -> mix -> band-limit FIR -> demodulate, integrated horizontally.
    vec3 accum_yiq = vec3(0.0);
    vec3 weight_sum = vec3(0.0);
    for (int tap = -CRT_FIR_RADIUS; tap <= CRT_FIR_RADIUS; tap++) {
        float tap_offset = float(tap);
        vec2 tap_uv = signal_uv + vec2(tap_offset * texel.x, 0.0);
        vec3 tap_yiq = CRT_RGB_TO_YIQ * texture(iChannel0, tap_uv).rgb;
        float tap_phase = crt_subcarrier_phase(src_px.x + tap_offset, src_px.y, frame_count);
        float carrier_cos = cos(tap_phase);
        float carrier_sin = sin(tap_phase);
        float chroma_mod = tap_yiq.y * carrier_cos + tap_yiq.z * carrier_sin;
        // Imperfect Y/C separation: chroma leaks into luma, luma leaks into chroma.
        float composite_luma = tap_yiq.x + chroma_mod * COMPOSITE_FRINGING;
        float composite_chroma = chroma_mod + tap_yiq.x * artifact_gain;
        vec3 tap_weight = vec3(
            crt_gaussian_weight(tap_offset, Y_BAND_PX),
            crt_gaussian_weight(tap_offset - CHROMA_DELAY_PX, I_BAND_PX),
            crt_gaussian_weight(tap_offset - CHROMA_DELAY_PX, Q_BAND_PX));
        // Demodulate by re-multiplying the carrier and doubling (2cos^2 = 1+cos2phi).
        accum_yiq += vec3(
            composite_luma,
            composite_chroma * 2.0 * carrier_cos,
            composite_chroma * 2.0 * carrier_sin) * tap_weight;
        weight_sum += tap_weight;
    }
    vec3 yiq_signal = accum_yiq / max(weight_sum, vec3(0.0001));

    // Analog noise, independent per Y/I/Q channel.
    yiq_signal.x += (crt_hash(vec3(src_px, frame_count)) - 0.5) * NOISE_LUMA;
    yiq_signal.y += (crt_hash(vec3(src_px, frame_count + 61.0)) - 0.5) * NOISE_CHROMA;
    yiq_signal.z += (crt_hash(vec3(src_px, frame_count + 123.0)) - 0.5) * NOISE_CHROMA;

    // AGC residual: low-frequency amplitude wobble only, to avoid harsh flicker.
    float gain_wave = sin(frame_count * 0.037 + source_line * 0.013) * 0.65
        + (crt_hash(vec3(floor(frame_count * 0.125), source_line * 0.031, 93.0)) - 0.5) * 0.35;
    yiq_signal *= 1.0 + RF_AMOUNT * RF_GAIN_WOBBLE * gain_wave;

    // Post-demod hue/saturation adjust in the I/Q plane.
    float hue_angle = HUE_DEG * CRT_TAU / 360.0;
    float hue_cos = cos(hue_angle);
    float hue_sin = sin(hue_angle);
    vec2 rotated_iq = vec2(
        yiq_signal.y * hue_cos - yiq_signal.z * hue_sin,
        yiq_signal.y * hue_sin + yiq_signal.z * hue_cos) * SATURATION;
    yiq_signal.y = rotated_iq.x;
    yiq_signal.z = rotated_iq.y;

    // RF snow: fine noise plus rare white impulses.
    float snow_hash = crt_hash(vec3(src_px * vec2(1.7, 2.3), frame_count + 211.0));
    float snow_impulse = max((snow_hash - 0.985) / 0.015, 0.0);
    yiq_signal.x += RF_AMOUNT * RF_SNOW * ((snow_hash - 0.5) * 0.35 + snow_impulse * 1.2);
    yiq_signal.y += RF_AMOUNT * RF_SNOW * snow_impulse * 0.20;
    yiq_signal.z -= RF_AMOUNT * RF_SNOW * snow_impulse * 0.15;

    vec3 ntsc_rgb = CRT_YIQ_TO_RGB * yiq_signal;

    // Multipath ghost: two faint shifted copies, then normalize brightness.
    if (GHOST_STRENGTH > 0.0001) {
        vec2 ghost_uv_offset = vec2(GHOST_OFFSET_PX * texel.x, 0.0);
        ntsc_rgb += texture(iChannel0, signal_uv - ghost_uv_offset).rgb * GHOST_STRENGTH;
        ntsc_rgb += texture(iChannel0, signal_uv - ghost_uv_offset * 2.0).rgb * GHOST_STRENGTH * 0.35;
        ntsc_rgb /= 1.0 + GHOST_STRENGTH * 1.35;
    }

    // Gamma is applied in Stage 2; here only clamp and mix toward the source.
    ntsc_rgb = clamp(ntsc_rgb, vec3(0.0), vec3(1.0));
    float ntsc_mix = clamp(SIGNAL_AMOUNT * (1.0 - RGB_BYPASS_MIX), 0.0, 1.0);
    fragColor = vec4(mix(source_rgb, ntsc_rgb, ntsc_mix), 1.0);
}
