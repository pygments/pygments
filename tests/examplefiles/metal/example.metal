#include <metal_stdlib>
#include <metal_tensor>
using namespace metal;

struct VertexOut {
    float4 position [[position]];
    half3 color;
};

vertex VertexOut vertex_main(uint vid [[vertex_id]],
                       constant packed_float3 *pos [[buffer(0)]]) {
    VertexOut out;
    out.position = float4(pos[vid], 1.0);
    out.color = half3(1.0h);
    return out;
}

fragment half4 fragment_main(VertexOut in [[stage_in]],
                     texture2d<half, access::sample> tex [[texture(0)]],
                     sampler s [[sampler(0)]]) {
    return tex.sample(s, float2(0.5));
}

kernel void kernel_main(device float *a [[buffer(0)]],
              threadgroup float *tile [[threadgroup(0)]],
              tensor<device half, dextents<int32_t, 2>> t [[buffer(1)]],
              uint i [[thread_position_in_grid]]) {
    if constexpr (sizeof(half) == 2) {
        float3x3 m = float3x3(1.0);
        a[i] = m[0][0] + tile[i];
    }
}
