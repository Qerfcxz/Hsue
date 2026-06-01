struct VSInput {
    float4 color:TEXCOORD0;
    float2 position:TEXCOORD1;
};
struct VSOutput {
    float4 color:COLOR;
    float4 position:SV_Position;
};
VSOutput main(VSInput input) {
    VSOutput output;
    output.color=input.color;
    output.position=float4(input.position,0,1);
    return output;
}