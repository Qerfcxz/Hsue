cbuffer Window:register(b0,space1) {
    float2 window_size;
    float font_size;
    float pixel_range;
}
struct VSInput {
    uint parameter_id:TEXCOORD0;
    float font_size:TEXCOORD1;
    float2 xy:TEXCOORD2;
    float2 uv:TEXCOORD3;
    float4 color:TEXCOORD4;
};
struct VSOutput {
    float4 color:TEXCOORD0;
    float4 position:SV_Position;
    float2 uv:TEXCOORD1;
    float4 border:TEXCOORD2;
    float font_size:TEXCOORD3;
    float2 screen:TEXCOORD4;
    float scale:TEXCOORD5;
    float border_flag:TEXCOORD6;
};
struct Parameter {
    float2 xy;
    float2 matrix_x;
    float2 matrix_y;
    float parameter_padding;
    float border_flag;
    float4 border;
};
StructuredBuffer<Parameter> parameter_buffer:register(t0,space0);
float2 apply_matrix(float2 matrix_xy,float2 matrix_x,float2 matrix_y,float2 xy) {
    float2 new_xy=xy-matrix_xy;
    return matrix_xy+float2(matrix_x.x*new_xy.x+matrix_x.y*new_xy.y,matrix_y.x*new_xy.x+matrix_y.y*new_xy.y);
}
VSOutput main(VSInput input) {
    VSOutput output;
    output.color=input.color;
    Parameter parameter=parameter_buffer[input.parameter_id];
    float2 new_xy=apply_matrix(parameter.xy,parameter.matrix_x,parameter.matrix_y,input.xy);
    output.position=float4(2*new_xy.x/window_size.x,2*new_xy.y/window_size.y,0,1);
    output.uv=input.uv;
    output.border=parameter.border;
    output.font_size=input.font_size;
    output.screen=input.xy;
    float matrix_scale=length(parameter.matrix_x);
    output.scale=(pixel_range*input.font_size*matrix_scale)/font_size;
    output.border_flag=parameter.border_flag;
    return output;
}