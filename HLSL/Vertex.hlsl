cbuffer Window:register(b0,space1) {
    float2 size;
    float font_size;
    float pixel_range;
}
struct VSInput {
    float4 color:TEXCOORD0;
    float2 xy:TEXCOORD1;
    float2 uv:TEXCOORD2;
    float parameter_id:TEXCOORD3;
    float size:TEXCOORD4;
};
struct VSOutput {
    float4 color:TEXCOORD0;
    float4 xy:SV_Position;
    float2 uv:TEXCOORD1;
    float4 this_border:TEXCOORD2;
    float size:TEXCOORD3;
    float2 screen:TEXCOORD4;
    float scale:TEXCOORD5;
    float border_flag:TEXCOORD6;
};
struct Parameter {
    float2 xy;
    float2 x;
    float2 y;
    float parameter_padding;
    float border_flag;
    float4 this_border;
};
StructuredBuffer<Parameter> parameter_buffer:register(t0,space0);
float2 apply_matrix(float2 matrix_xy,float2 matrix_x,float2 matrix_y,float2 xy) {
    float2 new_xy=xy-matrix_xy;
    return matrix_xy+float2(matrix_x.x*new_xy.x+matrix_x.y*new_xy.y,matrix_y.x*new_xy.x+matrix_y.y*new_xy.y);
}
VSOutput main(VSInput input) {
    VSOutput output;
    output.color=input.color;
    Parameter parameter=parameter_buffer[(uint)input.parameter_id];
    float2 new_input=apply_matrix(parameter.xy,parameter.x,parameter.y,input.xy);
    output.xy=float4(2*new_input.x/size.x,2*new_input.y/size.y,0,1);
    output.uv=input.uv;
    output.this_border=parameter.this_border;
    output.size=input.size;
    output.screen=input.xy;
    float matrix_scale=length(parameter.x);
    output.scale=(pixel_range*input.size*matrix_scale)/font_size;
    output.border_flag=parameter.border_flag;
    return output;
}