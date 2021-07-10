// A few random snippets of HLSL shader code I gathered...

[numthreads(256, 1, 1)]
void cs_main(uint3 threadId : SV_DispatchThreadID)
{
	// Seed the PRNG using the thread ID
	rng_state = threadId.x;

	// Generate a few numbers...
	uint r0 = rand_xorshift();
	uint r1 = rand_xorshift();
	// Do some stuff with them...

	// Generate a random float in [0, 1)...
	float f0 = float(rand_xorshift()) * (1.0 / 4294967296.0);

	// ...etc.
}

// Constant buffer of parameters
cbuffer IntegratorParams : register(b0)
{
	float2 specPow;		// Spec powers in XY directions (equal for isotropic BRDFs)
	float3 L;			// Unit vector toward light 
	int2 cThread;		// Total threads launched in XY dimensions
	int2 xyOutput;		// Where in the output buffer to store the result
}

static const float pi = 3.141592654;

float AshikhminShirleyNDF(float3 H)
{
	float normFactor = sqrt((specPow.x + 2.0f) * (specPow.y + 2.0)) * (0.5f / pi);
	float NdotH = H.z;
	float2 Hxy = normalize(H.xy);
	return normFactor * pow(NdotH, dot(specPow, Hxy * Hxy));
}

float BeckmannNDF(float3 H)
{
	float glossFactor = specPow.x * 0.5f + 1.0f;	// This is 1/m^2 in the usual Beckmann formula
	float normFactor = glossFactor * (1.0f / pi);
	float NdotHSq = H.z * H.z;
	return normFactor / (NdotHSq * NdotHSq) * exp(glossFactor * (1.0f - 1.0f / NdotHSq));
}

// Output buffer for compute shader (actually float, but must be declared as uint
// for atomic operations to work)
globallycoherent RWTexture2D<uint> o_data : register(u0);

// Sum up the outputs of all threads and store to the output location
static const uint threadGroupSize2D = 16;
static const uint threadGroupSize1D = threadGroupSize2D * threadGroupSize2D;
groupshared float g_partialSums[threadGroupSize1D];
void SumAcrossThreadsAndStore(float value, uint iThreadInGroup)
{
	// First reduce within the threadgroup: partial sums of 2, 4, 8... elements
	// are calculated by 1/2, 1/4, 1/8... of the threads, always keeping the
	// active threads at the front of the group to minimize divergence.

	// NOTE: there are faster ways of doing this...but this is simple to code
	// and good enough.

	g_partialSums[iThreadInGroup] = value;
	GroupMemoryBarrierWithGroupSync();

	[unroll] for (uint i = threadGroupSize1D / 2; i > 0; i /= 2)
	{
		if (iThreadInGroup < i)
		{
			g_partialSums[iThreadInGroup] += g_partialSums[iThreadInGroup + i];
		}
		GroupMemoryBarrierWithGroupSync();
	}

	// Then reduce across threadgroups: one thread from each group adds the group
	// total to the final output location, using a software transactional memory
	// style since D3D11 doesn't support atomic add on floats.
	// (Assumes the output value has been cleared to zero beforehand.)

	if (iThreadInGroup == 0)
	{
		float threadGroupSum = g_partialSums[0];
		uint outputValueRead = o_data[xyOutput];
		while (true)
		{
			uint newOutputValue = asuint(asfloat(outputValueRead) + threadGroupSum);
			uint previousOutputValue;
			InterlockedCompareExchange(
				o_data[xyOutput], outputValueRead, newOutputValue, previousOutputValue);
			if (previousOutputValue == outputValueRead)
				break;
			outputValueRead = previousOutputValue;
		}
	}
}

void main(
	in Vertex i_vtx,
	out Vertex o_vtx,
	out float3 o_vecCamera : CAMERA,
	out float4 o_uvzwShadow : UVZW_SHADOW,
	out float4 o_posClip : SV_Position)
{
	o_vtx = i_vtx;
	o_vecCamera = g_posCamera - i_vtx.m_pos;
	o_uvzwShadow = mul(float4(i_vtx.m_pos, 1.0), g_matWorldToUvzwShadow);
	o_posClip = mul(float4(i_vtx.m_pos, 1.0), g_matWorldToClip);
}

#pragma pack_matrix(row_major)

struct Vertex
{
	float3		m_pos		: POSITION;
	float3		m_normal	: NORMAL;
	float2		m_uv		: UV;
};

cbuffer CBFrame : CB_FRAME					// matches struct CBFrame in test.cpp
{
	float4x4	g_matWorldToClip;
	float4x4	g_matWorldToUvzwShadow;
	float3x3	g_matWorldToUvzShadowNormal;
	float3		g_posCamera;

	float3		g_vecDirectionalLight;
	float3		g_rgbDirectionalLight;

	float2		g_dimsShadowMap;
	float		g_normalOffsetShadow;
	float		g_shadowSharpening;

	float		g_exposure;					// Exposure multiplier
}

Texture2D<float3> g_texDiffuse : register(t0);
SamplerState g_ss : register(s0);

void main(
	in Vertex i_vtx,
	in float3 i_vecCamera : CAMERA,
	in float4 i_uvzwShadow : UVZW_SHADOW,
	out float3 o_rgb : SV_Target)
{
	float3 normal = normalize(i_vtx.m_normal);

	// Sample shadow map
	float shadow = EvaluateShadow(i_uvzwShadow, normal);

	// Evaluate diffuse lighting
	float3 diffuseColor = g_texDiffuse.Sample(g_ss, i_vtx.m_uv);
	float3 diffuseLight = g_rgbDirectionalLight * (shadow * saturate(dot(normal, g_vecDirectionalLight)));
	diffuseLight += SimpleAmbient(normal);

	o_rgb = diffuseColor * diffuseLight;
}

[domain("quad")]
void ds(
	in float edgeFactors[4] : SV_TessFactor,
	in float insideFactors[2] : SV_InsideTessFactor,
	in OutputPatch<VData, 4> inp,
	in float2 uv : SV_DomainLocation,
	out float4 o_pos : SV_Position)
{
	o_pos = lerp(lerp(inp[0].pos, inp[1].pos, uv.x), lerp(inp[2].pos, inp[3].pos, uv.x), uv.y);
}
