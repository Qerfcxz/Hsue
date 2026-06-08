cbuffer Window:register(b0,space1) {
    float2 size;
    float2 padding;
}
struct VSInput {
    float4 color:TEXCOORD0;
    float2 xy:TEXCOORD1;
    float2 uv:TEXCOORD2;
};
struct VSOutput {
    float4 color:COLOR;
    float4 xy:SV_Position;
    float2 uv:TEXCOORD0;
};
VSOutput main(VSInput input) {
    VSOutput output;
    output.color=input.color;
    output.xy=float4(2*(input.xy.x/size.x),2*(input.xy.y/size.y),0,1);
    output.uv=input.uv;
    return output;
}