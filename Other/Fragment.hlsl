Texture2D texture:register(t0,space2);
SamplerState this_sampler:register(s0,space2);
struct VSOutput {
    float4 color:COLOR;
    float4 xy:SV_Position;
    float2 uv:TEXCOORD0;
};
float4 main(VSOutput output):SV_Target {
    return output.color*texture.Sample(this_sampler,output.uv);
}