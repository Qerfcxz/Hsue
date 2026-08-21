struct VSOutput {
    float4 position:SV_Position;
    float2 uv:TEXCOORD0;
};

VSOutput main(uint vertex_id:SV_VertexID) {
    VSOutput output;
    output.uv=float2((vertex_id<<1)&2,vertex_id&2);
    output.position=float4(output.uv*2-1,0,1);
    output.position.y=-output.position.y;
    return output;
}