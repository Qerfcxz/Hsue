Texture2D texture:register(t0,space2);
SamplerState this_sampler:register(s0,space2);
struct VSOutput {
    float4 color:TEXCOORD0;
    float4 xy:SV_Position;
    float2 uv:TEXCOORD1;
    float4 clip:TEXCOORD2;
    float size:TEXCOORD3;
    float2 screen:TEXCOORD4;
    float scale:TEXCOORD5;
    float clip_flag:TEXCOORD6;
};
float4 main(VSOutput output):SV_Target {
    float out_of=0;
    if (output.clip_flag>0) {
        out_of=(output.screen.x<output.clip.x||output.screen.y<output.clip.y||output.screen.x>output.clip.z||output.screen.y>output.clip.w)?1:0;
    }
    float4 texture_color=texture.Sample(this_sampler,output.uv);
    float4 final_color;
    if (output.size>0) {
        float sd=max(min(texture_color.r,texture_color.g),min(max(texture_color.r,texture_color.g),texture_color.b))-0.5;
        final_color=float4(output.color.rgb,output.color.a*clamp(sd*output.scale+0.5,0,1));
    } else {
        final_color=output.color*texture_color;
    }
    if (out_of>0) {
        discard;
    }
    return final_color;
}