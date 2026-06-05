struct VSOutput {
    float4 color:COLOR;
    float4 position:SV_Position;
};
float4 main(VSOutput output):SV_Target {
    return output.color;
}