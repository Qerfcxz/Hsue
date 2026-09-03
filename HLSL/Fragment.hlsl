Texture2D this_texture:register(t0,space2);
SamplerState this_sampler:register(s0,space2);
struct VSOutput {
    float4 color:TEXCOORD0;
    float4 position:SV_Position;
    float2 uv:TEXCOORD1;
    float4 border:TEXCOORD2;
    float font_size:TEXCOORD3;
    float2 screen:TEXCOORD4;
    float scale:TEXCOORD5;
    float enable:TEXCOORD6;
};
float4 main(VSOutput input):SV_Target {
    if (input.enable>0) {
        if (input.screen.x<input.border.x||input.screen.y<input.border.y||input.screen.x>input.border.z||input.screen.y>input.border.w) {
            discard;
        }
    }
    float4 texture_color=this_texture.Sample(this_sampler,input.uv);
    float4 final_color;
    if (input.font_size>0) {
        float signed_distance=max(min(texture_color.r,texture_color.g),min(max(texture_color.r,texture_color.g),texture_color.b))-0.5;
        final_color=float4(input.color.rgb,input.color.a*saturate(signed_distance*input.scale+0.5));
    } else {
        final_color=input.color*texture_color;
    }
    return final_color;
}