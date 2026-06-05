cbuffer Window:register(b0,space1) {
    float2 size;
    float2 padding;
}
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
    output.position=float4(2*(input.position.x/size.x),2*(input.position.y/size.y),0,1);
    return output;
}