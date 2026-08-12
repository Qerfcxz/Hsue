struct VSOutput {
    float4 Position:SV_Position;
    float2 UV:TEXCOORD0;
};

VSOutput main(uint id:SV_VertexID) {
    VSOutput output;
    output.UV=float2((id<<1)&2,id&2);
    output.Position=float4(output.UV*2-1,0,1);
    output.Position.y=-output.Position.y;
    return output;
}