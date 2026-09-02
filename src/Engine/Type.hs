{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Engine.Type where

import qualified SDL.Type as SDLT
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.Bits as DB
import qualified Data.Hashable as DH
import qualified Data.HashMap.Strict as DHMS
import qualified Data.HashSet as DHS
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Vector as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

newtype Dynamic_bool a=Dynamic_bool {dynamic_bool::(Int->Int)->Event a->Engine a->Widget a->Bool}

instance Eq (Dynamic_bool a) where
    (==)=dynamic_bool_equal

dynamic_bool_equal::ET.Has_call_stack=>Dynamic_bool a->Dynamic_bool a->Bool
dynamic_bool_equal _ _=EF.empty_error

instance DB.Bits (Dynamic_bool a) where
    (.&.)=dynamic_bool_and
    (.|.)=dynamic_bool_or
    xor=dynamic_bool_xor
    complement=dynamic_bool_complement
    shift=dynamic_bool_shift
    rotate=dynamic_bool_rotate
    bitSize=dynamic_bool_bit_size
    bitSizeMaybe=dynamic_bool_bit_size_maybe
    isSigned=dynamic_bool_is_signed
    testBit=dynamic_bool_test_bit
    bit=dynamic_bool_bit
    popCount=dynamic_bool_pop_count

dynamic_bool_and::ET.Has_call_stack=>Dynamic_bool a->Dynamic_bool a->Dynamic_bool a
dynamic_bool_and first_dynamic_bool second_dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_binary_operator (DB..&.) first_dynamic_bool.dynamic_bool second_dynamic_bool.dynamic_bool}

dynamic_bool_or::ET.Has_call_stack=>Dynamic_bool a->Dynamic_bool a->Dynamic_bool a
dynamic_bool_or first_dynamic_bool second_dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_binary_operator (DB..|.) first_dynamic_bool.dynamic_bool second_dynamic_bool.dynamic_bool}

dynamic_bool_xor::ET.Has_call_stack=>Dynamic_bool a->Dynamic_bool a->Dynamic_bool a
dynamic_bool_xor first_dynamic_bool second_dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_binary_operator DB.xor first_dynamic_bool.dynamic_bool second_dynamic_bool.dynamic_bool}

dynamic_bool_complement::ET.Has_call_stack=>Dynamic_bool a->Dynamic_bool a
dynamic_bool_complement dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_unary_operator DB.complement dynamic_bool.dynamic_bool}

dynamic_bool_shift::ET.Has_call_stack=>Dynamic_bool a->Int->Dynamic_bool a
dynamic_bool_shift dynamic_bool int=if int==0 then dynamic_bool else Dynamic_bool {dynamic_bool=dynamic_bool_false}

dynamic_bool_rotate::ET.Has_call_stack=>Dynamic_bool a->Int->Dynamic_bool a
dynamic_bool_rotate dynamic_bool _=dynamic_bool

dynamic_bool_bit_size::ET.Has_call_stack=>Dynamic_bool a->Int
dynamic_bool_bit_size _=1

dynamic_bool_bit_size_maybe::ET.Has_call_stack=>Dynamic_bool a->Maybe Int
dynamic_bool_bit_size_maybe _=Just 1

dynamic_bool_is_signed::ET.Has_call_stack=>Dynamic_bool a->Bool
dynamic_bool_is_signed _=False

dynamic_bool_test_bit::ET.Has_call_stack=>Dynamic_bool a->Int->Bool
dynamic_bool_test_bit _ _=EF.empty_error

dynamic_bool_bit::ET.Has_call_stack=>Int->Dynamic_bool a
dynamic_bool_bit int=if int==0 then Dynamic_bool {dynamic_bool=dynamic_bool_true} else Dynamic_bool {dynamic_bool=dynamic_bool_false}

dynamic_bool_pop_count::ET.Has_call_stack=>Dynamic_bool a->Int
dynamic_bool_pop_count _=EF.empty_error

dynamic_bool_true::ET.Has_call_stack=>(Int->Int)->Event a->Engine a->Widget a->Bool
dynamic_bool_true _ _ _ _=True

dynamic_bool_false::ET.Has_call_stack=>(Int->Int)->Event a->Engine a->Widget a->Bool
dynamic_bool_false _ _ _ _=False

dynamic_bool_unary_operator::ET.Has_call_stack=>(Bool->Bool)->((Int->Int)->Event a->Engine a->Widget a->Bool)->(Int->Int)->Event a->Engine a->Widget a->Bool
dynamic_bool_unary_operator operator dynamic_bool getter event engine widget=operator (dynamic_bool getter event engine widget)

dynamic_bool_binary_operator::ET.Has_call_stack=>(Bool->Bool->Bool)->((Int->Int)->Event a->Engine a->Widget a->Bool)->((Int->Int)->Event a->Engine a->Widget a->Bool)->(Int->Int)->Event a->Engine a->Widget a->Bool
dynamic_bool_binary_operator operator first_dynamic_bool second_dynamic_bool getter event engine widget=operator (first_dynamic_bool getter event engine widget) (second_dynamic_bool getter event engine widget)

newtype Dynamic_int a=Dynamic_int {dynamic_int::(Int->Int)->Event a->Engine a->Widget a->Int}

instance Num (Dynamic_int a) where
    (+)=dynamic_int_addition
    (*)=dynamic_int_multiplication
    abs=dynamic_int_abs
    signum=dynamic_int_signum
    fromInteger=dynamic_int_from_integer
    negate=dynamic_int_negate

dynamic_int_addition::ET.Has_call_stack=>Dynamic_int a->Dynamic_int a->Dynamic_int a
dynamic_int_addition first_dynamic_int second_dynamic_int=Dynamic_int {dynamic_int=dynamic_int_binary_operator (+) first_dynamic_int.dynamic_int second_dynamic_int.dynamic_int}

dynamic_int_multiplication::ET.Has_call_stack=>Dynamic_int a->Dynamic_int a->Dynamic_int a
dynamic_int_multiplication first_dynamic_int second_dynamic_int=Dynamic_int {dynamic_int=dynamic_int_binary_operator (*) first_dynamic_int.dynamic_int second_dynamic_int.dynamic_int}

dynamic_int_abs::ET.Has_call_stack=>Dynamic_int a->Dynamic_int a
dynamic_int_abs dynamic_int=Dynamic_int {dynamic_int=dynamic_int_unary_operator abs dynamic_int.dynamic_int}

dynamic_int_signum::ET.Has_call_stack=>Dynamic_int a->Dynamic_int a
dynamic_int_signum dynamic_int=Dynamic_int {dynamic_int=dynamic_int_unary_operator signum dynamic_int.dynamic_int}

dynamic_int_from_integer::ET.Has_call_stack=>Integer->Dynamic_int a
dynamic_int_from_integer integer=Dynamic_int {dynamic_int=const (const (const (const (fromInteger integer))))}

dynamic_int_negate::ET.Has_call_stack=>Dynamic_int a->Dynamic_int a
dynamic_int_negate dynamic_int=Dynamic_int {dynamic_int=dynamic_int_unary_operator negate dynamic_int.dynamic_int}

dynamic_int_unary_operator::ET.Has_call_stack=>(Int->Int)->((Int->Int)->Event a->Engine a->Widget a->Int)->(Int->Int)->Event a->Engine a->Widget a->Int
dynamic_int_unary_operator operator dynamic_int getter event engine widget=operator (dynamic_int getter event engine widget)

dynamic_int_binary_operator::ET.Has_call_stack=>(Int->Int->Int)->((Int->Int)->Event a->Engine a->Widget a->Int)->((Int->Int)->Event a->Engine a->Widget a->Int)->(Int->Int)->Event a->Engine a->Widget a->Int
dynamic_int_binary_operator operator first_dynamic_int second_dynamic_int getter event engine widget=operator (first_dynamic_int getter event engine widget) (second_dynamic_int getter event engine widget)

newtype Raw_coroutine a b=Raw_coroutine {iterator::Int->(Int,DS.Seq (Coroutine a),b)}

instance Functor (Raw_coroutine a) where
    fmap=raw_coroutine_fmap

raw_coroutine_fmap::ET.Has_call_stack=>(a->b)->Raw_coroutine c a->Raw_coroutine c b
raw_coroutine_fmap function raw_coroutine=Raw_coroutine {iterator=raw_coroutine_fmap_a function raw_coroutine.iterator}

raw_coroutine_fmap_a::ET.Has_call_stack=>(a->b)->(Int->(Int,DS.Seq (Coroutine c),a))->Int->(Int,DS.Seq (Coroutine c),b)
raw_coroutine_fmap_a function iterator int=let (new_int,coroutine_sequence,value)=iterator int in (new_int,coroutine_sequence,function value)

instance Applicative (Raw_coroutine a) where
    pure=raw_coroutine_pure
    (<*>)=raw_coroutine_apply

raw_coroutine_pure::ET.Has_call_stack=>a->Raw_coroutine b a
raw_coroutine_pure value=Raw_coroutine {iterator=raw_coroutine_pure_a value}

raw_coroutine_pure_a::ET.Has_call_stack=>a->Int->(Int,DS.Seq (Coroutine b),a)
raw_coroutine_pure_a value int=(int,DS.empty,value)

raw_coroutine_apply::ET.Has_call_stack=>Raw_coroutine a (b->c)->Raw_coroutine a b->Raw_coroutine a c
raw_coroutine_apply first_raw_coroutine second_raw_coroutine=Raw_coroutine {iterator=raw_coroutine_apply_a first_raw_coroutine.iterator second_raw_coroutine.iterator}

raw_coroutine_apply_a::ET.Has_call_stack=>(Int->(Int,DS.Seq (Coroutine a),b->c))->(Int->(Int,DS.Seq (Coroutine a),b))->Int->(Int,DS.Seq (Coroutine a),c)
raw_coroutine_apply_a function_iterator value_iterator int=let (function_int,function_coroutine_sequence,function)=function_iterator int in
    let (value_int,value_coroutine_sequence,value)=value_iterator function_int in (value_int,function_coroutine_sequence DS.>< value_coroutine_sequence,function value)

instance Monad (Raw_coroutine a) where
    return=pure
    (>>=)=raw_coroutine_bind

raw_coroutine_bind::ET.Has_call_stack=>Raw_coroutine a b->(b->Raw_coroutine a c)->Raw_coroutine a c
raw_coroutine_bind raw_coroutine function=Raw_coroutine {iterator=raw_coroutine_bind_a raw_coroutine.iterator function}

raw_coroutine_bind_a::ET.Has_call_stack=>(Int->(Int,DS.Seq (Coroutine a),b))->(b->Raw_coroutine a c)->Int->(Int,DS.Seq (Coroutine a),c)
raw_coroutine_bind_a iterator function int=let (new_int,coroutine_sequence,value)=iterator int in let (new_new_int,new_coroutine_sequence,new_value)=(function value).iterator new_int in (new_new_int,coroutine_sequence DS.>< new_coroutine_sequence,new_value)

data Trigger_result a b=Trigger_result {next::Event a->Engine a->Maybe Int,update::Engine a->Engine a,value::b}

instance Functor (Trigger_result a) where
    fmap=trigger_result_fmap

trigger_result_fmap::ET.Has_call_stack=>(a->b)->Trigger_result c a->Trigger_result c b
trigger_result_fmap function trigger_result=case trigger_result of
    Trigger_result {next,update,value}->Trigger_result {next=next,update=update,value=function value}

instance Applicative (Trigger_result a) where
    pure=trigger_result_pure
    (<*>)=trigger_result_apply

trigger_result_pure::ET.Has_call_stack=>a->Trigger_result b a
trigger_result_pure _=EF.empty_error

trigger_result_apply::ET.Has_call_stack=>Trigger_result a (b->c)->Trigger_result a b->Trigger_result a c
trigger_result_apply _ _=EF.empty_error

data Engine a=Engine {custom::Custom_state a,main_id::Event a->Engine a->Maybe Int,projection_strategy::Event a->Engine a->Projection_strategy,callback::FP.FunPtr (FP.Ptr ()->DW.Word32->DW.Word64->IO DW.Word64),device::FP.Ptr SDLT.SDL_GPUDevice,texture::FP.Ptr SDLT.SDL_GPUTexture,sampler::DIM.IntMap (FP.Ptr SDLT.SDL_GPUSampler),default_sampler::FP.Ptr SDLT.SDL_GPUSampler,canvas_graphics_pipeline::FP.Ptr SDLT.SDL_GPUGraphicsPipeline,pipeline::DIM.IntMap Pipeline,shader::DIM.IntMap Shader,default_shader::FP.Ptr SDLT.SDL_GPUShader,vertex_shader::FP.Ptr SDLT.SDL_GPUShader,fragment_shader::FP.Ptr SDLT.SDL_GPUShader,vertex_buffer::FP.Ptr SDLT.SDL_GPUBuffer,index_buffer::FP.Ptr SDLT.SDL_GPUBuffer,parameter_buffer::FP.Ptr SDLT.SDL_GPUBuffer,transfer_buffer::FP.Ptr SDLT.SDL_GPUTransferBuffer,picture_transfer_buffer::FP.Ptr SDLT.SDL_GPUTransferBuffer,atlas::Atlas,canvas::DIM.IntMap Canvas,album::DIM.IntMap Album,leaf::DIM.IntMap (Projection a),node::DIM.IntMap (Node a),window::DIM.IntMap Window,font::DIM.IntMap Font,atlas_font::DIM.IntMap Atlas_font,window_map::DHMS.HashMap DW.Word32 Int,font_map::DHMS.HashMap String Int,system_cursor_map::DHMS.HashMap System_cursor (FP.Ptr SDLT.SDL_Cursor),request::DS.Seq (Request a),key::DHS.HashSet Key,u::FCT.CFloat,v::FCT.CFloat,font_size::FCT.CFloat,pixel_range::FCT.CFloat,max_picture_size::FCT.CInt,max_vertex_size::DW.Word32,max_index_size::DW.Word32,max_parameter_size::DW.Word32,padding::DW.Word32,event_number::DW.Word32,time::DW.Word64,timer::Timer,count::Int,initial_canvas_id::Int,canvas_id::Int,initial_album_id::Int,album_id::Int,initial_font_id::Int,font_id::Int,exponent_width::Int,exponent_height::Int}

data Projection a=Without {ancestry_id::DS.Seq Int,object::Widget a}|With {ancestry_id::DS.Seq Int,object::Widget a,image::Widget a}

data Node a=Node {ancestry_id::DS.Seq Int,leaf_child::DIS.IntSet,node_child::DIS.IntSet,event_transform::Engine a->Event a->Event a,widget_transform::Event a->Engine a->Widget a->Widget a}

data Widget a=Group {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,index::Int,group_widget::DIM.IntMap (Widget a)}|Vector {index::Int,vector_widget::DV.Vector (Widget a)}|Trigger {next::Event a->Engine a->Maybe Int,trigger::Event a->Engine a->Engine a}|Io_trigger {next::Event a->Engine a->Maybe Int,io_trigger::Event a->Engine a->IO (Engine a)}|Mix_trigger {next::Event a->Engine a->Maybe Int,mix_trigger::Event a->(Engine a->Engine a,Engine a->IO (Engine a)),order::Bool}|Widget_trigger {next::Event a->Engine a->Maybe Int,widget_trigger::Event a->Engine a->Widget a->(Widget a,Engine a->Engine a),widget::Widget a}|Widget_io_trigger {next::Event a->Engine a->Maybe Int,widget_io_trigger::Event a->Engine a->Widget a->(Widget a,Engine a->IO (Engine a)),widget::Widget a}|Widget_mix_trigger {next::Event a->Engine a->Maybe Int,widget_mix_trigger::Event a->Engine a->Widget a->(Widget a,Engine a->Engine a,Engine a->IO (Engine a)),order::Bool,widget::Widget a}|Coroutine {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,index::Int,variable_size::Int,user_variable_size::Int,coroutine_state::DIM.IntMap (Coroutine_state a),layout::DVS.Vector Layout,linear_coroutine::DV.Vector (Linear_coroutine a),iterative::Bool}|Store {store::Data}|Collector {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,submit::DIM.IntMap (DS.Seq (Submit a))}|Group_visual {arrange::Arrange,group_visual::DIM.IntMap (Visual a)}|Vector_visual {arrange::Arrange,vector_visual::DV.Vector (Visual a)}

data Widget_request a=Group_request {initial_min_index::Int,initial_max_index::Int,index::Int,insert_widget_request::DS.Seq (Insert (Widget_request a))}|Vector_request {index::Int,vector_widget_request::DS.Seq (Widget_request a)}|Trigger_request {next::Event a->Engine a->Maybe Int,trigger::Event a->Engine a->Engine a}|Io_trigger_request {next::Event a->Engine a->Maybe Int,io_trigger::Event a->Engine a->IO (Engine a)}|Mix_trigger_request {next::Event a->Engine a->Maybe Int,mix_trigger::Event a->(Engine a->Engine a,Engine a->IO (Engine a)),order::Bool}|Widget_trigger_request {next::Event a->Engine a->Maybe Int,widget_trigger::Event a->Engine a->Widget a->(Widget a,Engine a->Engine a),widget_request::Widget_request a}|Widget_io_trigger_request {next::Event a->Engine a->Maybe Int,widget_io_trigger::Event a->Engine a->Widget a->(Widget a,Engine a->IO (Engine a)),widget_request::Widget_request a}|Widget_mix_trigger_request {next::Event a->Engine a->Maybe Int,widget_mix_trigger::Event a->Engine a->Widget a->(Widget a,Engine a->Engine a,Engine a->IO (Engine a)),order::Bool,widget_request::Widget_request a}|Coroutine_request {initial_min_index::Int,initial_max_index::Int,index::Int,insert_widget_request::DS.Seq (Insert (Widget_request a)),raw_coroutine::Raw_coroutine a (),iterative::Bool}|Store_request {store::Data}|Collector_request {initial_min_index::Int,initial_max_index::Int}|Group_visual_request {arrange::Arrange,group_visual_request::DIM.IntMap (Visual_request a)}|Vector_visual_request {arrange::Arrange,vector_visual_request::DV.Vector (Visual_request a)}

data Visual a=Rectangle {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat}|Triangle {arrange::Arrange,first_point::Point,second_point::Point,third_point::Point}|Convex_polygon {arrange::Arrange,point_set::DS.Seq Point}|Regular_polygon {arrange::Arrange,number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,path::String,locked::Bool}|Large_picture {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat,album_id::Int}|Atlas {arrange::Arrange,path::String,clip_request::DS.Seq Clip_request,clip::DVS.Vector Clip,index::Int,locked::Bool}|Large_atlas {arrange::Arrange,clip::DVS.Vector Clip,index::Int,album_id::Int}|Animation {arrange::Arrange,delay::DVS.Vector FCT.CFloat,moment::FCT.CFloat,half_width::FCT.CFloat,half_height::FCT.CFloat,padding::FCT.CFloat,exponent_width::Int,exponent_height::Int,width_number::Int,height_number::Int,album_number::Int,count::Int,index::Int,album_id::Int}|Text {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat,current_y::FCT.CFloat,min_y::FCT.CFloat,max_y::FCT.CFloat,anchor::Anchor,article::DS.Seq (DS.Seq Row),charset::DHMS.HashMap String (DHS.HashSet Char),locked::Bool}|Editor {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat,cursor_width::FCT.CFloat,failure_advance::FCT.CFloat,failure_left::FCT.CFloat,failure_down::FCT.CFloat,failure_right::FCT.CFloat,failure_up::FCT.CFloat,current_y::FCT.CFloat,min_y::FCT.CFloat,max_y::FCT.CFloat,font_size::FCT.CFloat,text_color::Color,cursor_color::Color,box_color::Color,selected_color::Color,line_width::Int->FCT.CFloat,line_typesetting::Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat),anchor::Anchor,cursor::Cursor,line::DS.Seq Line,appended_typesetting::DS.Seq Typesetting,typesetting::DVS.Vector Typesetting,max_typesetting_size::Int,atlas_font_id::Int}|Canvas {arrange::Arrange,canvas_width::DW.Word32,canvas_height::DW.Word32,half_width::FCT.CFloat,half_height::FCT.CFloat,canvas_id::Int}|Custom_visual {custom::Custom_visual a}

data Visual_request a=Rectangle_request {arrange::Arrange,rectangle_width::FCT.CFloat,rectangle_height::FCT.CFloat}|Triangle_request {arrange::Arrange,first_point::Point,second_point::Point,third_point::Point}|Convex_polygon_request {arrange::Arrange,point_set::DS.Seq Point}|Regular_polygon_request {arrange::Arrange,number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture_request {arrange::Arrange,path::String}|Large_picture_request {arrange::Arrange,path::String}|Atlas_request {arrange::Arrange,path::String,clip_request::DS.Seq Clip_request}|Large_atlas_request {arrange::Arrange,path::String,clip_request::DS.Seq Clip_request}|Animation_request {arrange::Arrange,min_delay::FCT.CFloat,padding::Int,exponent_width::Int,exponent_height::Int,path::String}|Text_request {arrange::Arrange,text_width::FCT.CFloat,text_height::FCT.CFloat,calculate_width::DS.Seq Row->DS.Seq (DS.Seq Row)->Int->FCT.CFloat,calculate_typesetting::DS.Seq (DS.Seq Row)->Int->Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat),anchor::Anchor,article::DS.Seq (DS.Seq Sentence),load::Bool}|Editor_request {arrange::Arrange,editor_width::FCT.CFloat,editor_height::FCT.CFloat,cursor_width::FCT.CFloat,failure_advance::FCT.CFloat,failure_left::FCT.CFloat,failure_down::FCT.CFloat,failure_right::FCT.CFloat,failure_up::FCT.CFloat,font_size::FCT.CFloat,text_color::Color,cursor_color::Color,box_color::Color,selected_color::Color,line_width::Int->FCT.CFloat,line_typesetting::Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat),anchor::Anchor,max_typesetting_size::Int,atlas_font_id::Int}|Canvas_request {arrange::Arrange,canvas_width::DW.Word32,canvas_height::DW.Word32,maybe_canvas_id::Maybe Int}|Custom_visual_request {custom::Custom_visual_request a}

data Request a=Reset_timer {interval::DW.Word64}|Stop_timer|Stop_timer_safe|Create_widget {leaf_id::Int,maybe_father_id::Maybe Int,widget_request::Widget_request a}|Remove_widget {leaf_id::Int}|Create_node {node_id::Int,maybe_father_id::Maybe Int,event_transform::Engine a->Event a->Event a,widget_transform::Event a->Engine a->Widget a->Widget a}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::DT.Text,window_width::FCT.CInt,window_height::FCT.CInt,color::Color,window_flag::DHS.HashSet Window_flag,blend_state::Blend_state}|Remove_window {window_id::Int}|Create_canvas {canvas_width::DW.Word32,canvas_height::DW.Word32,maybe_canvas_id::Maybe Int}|Remove_canvas {canvas_id::Int}|Create_shader {shader_id::Int,stage::DW.Word32,num_sampler::DW.Word32,num_uniform_buffer::DW.Word32,path::String}|Remove_shader {shader_id::Int}|Create_pipeline {maybe_vertex_shader_id::Maybe Int,fragment_shader_id::Int,pipeline_id::Int,blend_state::Blend_state}|Remove_pipeline {pipeline_id::Int}|Create_sampler {sampler_id::Int,sampler_create_info::Sampler_create_info}|Remove_sampler {sampler_id::Int}|Create_atlas_font {atlas_font_id::Int,exponent_width::Int,exponent_height::Int,padding::DW.Word32,width::DW.Word32,height::DW.Word32,font_size::FCT.CFloat,pixel_range::FCT.CFloat,path::String,maybe_charset::Maybe (DHS.HashSet Char)}|Remove_atlas_font {atlas_font_id::Int}|Set_window_icon {window_id::Int,path::String}|Set_window_size {window_id::Int,window_width::FCT.CInt,window_height::FCT.CInt}|Set_window_position {window_id::Int,x::FCT.CInt,y::FCT.CInt}|Set_window_title {window_id::Int,title::DT.Text}|Set_window_fullscreen {window_id::Int,fullscreen::Bool}|Set_system_cursor {system_cursor::System_cursor}|Clean_atlas|Unlock {leaf_id::Int}|Update_font {path::String,maybe_charset::Maybe (DHS.HashSet Char)}|Update_atlas_font {atlas_font_id::Int,path::String,maybe_charset::Maybe (DHS.HashSet Char)}|Render {window_id::Int,render_selector::Selector (),projection_move::Projection_move,maybe_sampler_id::Maybe Int}|Canvas_render {canvas_id::Int,canvas_render_selector::Selector (),projection_move::Projection_move,maybe_sampler_id::Maybe Int}|Canvas_widget_render {projection_path::Projection_path,canvas_widget_render_selector::Selector (Selector ()),projection_move::Projection_move,maybe_sampler_id::Maybe Int}|Shader_canvas {uniform::Uniform,canvas_id::Int,pipeline_id::Int,maybe_sampler_id::Maybe Int}|Io {io::Engine a->IO (Engine a)}

data Submit a=Submit {submit_mode::Submit_mode,submit_data::Submit_data a,parameter::Parameter,vertex_size::DW.Word32,index_size::DW.Word32}

data Submit_data a=Submit_rectangle {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}|Submit_triangle {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,first_x::FCT.CFloat,first_y::FCT.CFloat,second_x::FCT.CFloat,second_y::FCT.CFloat,third_x::FCT.CFloat,third_y::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat}|Submit_convex_polygon {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,point_set::DS.Seq Point}|Submit_regular_polygon {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,angle::FCT.CFloat,radius::FCT.CFloat,number::Int}|Submit_text {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat,current_y::FCT.CFloat,ratio::FCT.CFloat,article::DS.Seq (DS.Seq Row)}|Custom_submit_data {custom::Custom_submit_data a}

data Coroutine_state a=Coroutine_state {widget::Widget a,variable::DVU.Vector Int,user_variable::DVU.Vector Int,program_counter::DIM.IntMap Program_counter,index_group::DIM.IntMap (DS.Seq Int),main_index_group::DS.Seq Int,index_group_index::Int,program_counter_index::Int}

data Coroutine a=Done|Emit {emit::Event a->Engine a->Widget a->(Widget a,Engine a->Engine a)}|Wait {dynamic_int::Dynamic_int a}|Forever {coroutine::Coroutine a}|Then {coroutine_sequence::DS.Seq (Coroutine a)}|While {dynamic_bool::Dynamic_bool a,coroutine::Coroutine a}|Pause {dynamic_bool::Dynamic_bool a,coroutine::Coroutine a}|Skip {dynamic_bool::Dynamic_bool a,coroutine::Coroutine a}|Assign {int::Int,dynamic_int::Dynamic_int a}|Repeat {dynamic_int::Dynamic_int a,coroutine::Coroutine a}|Clone {int::Int,coroutine::Coroutine a}|If {dynamic_bool::Dynamic_bool a,first_coroutine::Coroutine a,second_coroutine::Coroutine a}|Dynamic_clone {int::Int,dynamic_int::Dynamic_int a,coroutine::Coroutine a}|Case {int::Int,dynamic_int::Dynamic_int a,coroutine_sequence::DS.Seq (Coroutine a)}|Fork {int::Int,coroutine::Coroutine a,coroutine_sequence::DS.Seq (Coroutine a)}|Race {first_int::Int,second_int::Int,dynamic_int::Dynamic_int a,coroutine_sequence::DS.Seq (Coroutine a)}

data Linear_coroutine a=Linear_end|Linear_emit {emit::Event a->Engine a->Widget a->(Widget a,Engine a->Engine a)}|Linear_wait {int_index::Int}|Linear_countdown {int_index::Int}|Linear_wake {int_index::Int}|Linear_fork {code_index::Int}|Linear_yield {code_index::Int}|Linear_jump {code_index::Int}|Linear_kill {group_int_index::DIM.IntMap Int}|Linear_one_less_jump {int_index::Int,code_index::Int}|Linear_one_more_jump {int_index::Int,code_index::Int}|Linear_kill_clone {int_index::Int,clone_number::Int}|Linear_dynamic_int {int_index::Int,dynamic_int::Dynamic_int a}|Linear_true_jump {code_index::Int,dynamic_bool::Dynamic_bool a}|Linear_false_jump {code_index::Int,dynamic_bool::Dynamic_bool a}|Linear_less_jump {int::Int,int_index::Int,code_index::Int}|Linear_clone {int::Int,int_index::Int,clone_number::Int}|Linear_wake_group {int_index::Int,dynamic_int::Dynamic_int a}|Linear_assign {user_int_index::Int,clone_number::Int,dynamic_int::Dynamic_int a}|Linear_create_group {int::Int,first_int_index::Int,second_int_index::Int,group_code_index::DIM.IntMap Int}|Linear_create_active_group {int::Int,first_int_index::Int,second_int_index::Int,group_code_index::DIM.IntMap Int}|Linear_dynamic_clone {int::Int,int_index::Int,code_index::Int,clone_number::Int,dynamic_int::Dynamic_int a}

data Event a=Empty|Quit|Time {tick::Int,time::DW.Word64,interval::DW.Word64}|At {window_id::Int,action::Action}|Custom_event {custom::Custom_event a}

data Selector a=None_selector|Combine_selector {combine_selector::DS.Seq (Selector a)}|Self_selector {value::a}|All_selector {maybe_value::Maybe a,value::a}|Trigger_selector {maybe_value::Maybe a,value::a,bounded::Bool}|Default_selector {maybe_value::Maybe a,value::a,bounded::Bool}|Hosted_selector {maybe_value::Maybe a,selector::Selector a,bounded::Bool,strict::Bool}|Any_selector {maybe_value::Maybe a,selector::Selector a,strict::Bool}|Group_selector {maybe_value::Maybe a,group_selector::DIM.IntMap (Selector a),bounded::Bool,strict::Bool}|Vector_selector {maybe_value::Maybe a,vector_selector::DIM.IntMap (Selector a),bounded::Bool,strict::Bool}|Widget_trigger_selector {maybe_value::Maybe a,selector::Selector a,strict::Bool}|Widget_io_trigger_selector {maybe_value::Maybe a,selector::Selector a,strict::Bool}|Widget_mix_trigger_selector {maybe_value::Maybe a,selector::Selector a,strict::Bool}|Coroutine_selector {maybe_value::Maybe a,coroutine_selector::DIM.IntMap (Selector a),bounded::Bool,strict::Bool}

data Visual_selector a=None_visual_selector|Combine_visual_selector {combine_visual_selector::DS.Seq (Visual_selector a)}|Any_visual_selector {value::a,strict::Bool}|Group_visual_selector {group_value::DIM.IntMap a,bounded::Bool,strict::Bool}|Vector_visual_selector {vector_value::DIM.IntMap a,bounded::Bool,strict::Bool}

data Insert a=Insert {insert_strategy::Insert_strategy,value::a}

data Border a=Border {left::a,down::a,right::a,up::a}

data Vertex=Vertex {parameter_id::DW.Word32,font_size::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Uniform=Uniform {size::Int,alignment::Int,write::FP.Ptr ()->IO ()}

data Shader=Shader {sdl_shader::FP.Ptr SDLT.SDL_GPUShader,reference::Int}

data Pipeline=Pipeline {sdl_pipeline::FP.Ptr SDLT.SDL_GPUGraphicsPipeline,vertex_shader_id::Int,fragment_shader_id::Int}|Default_pipeline {sdl_pipeline::FP.Ptr SDLT.SDL_GPUGraphicsPipeline,fragment_shader_id::Int}

data Program_counter=Program_counter {code_index::Int,clone_index::Int}

data Canvas=Free_canvas {width::DW.Word32,height::DW.Word32,half_width::FCT.CFloat,half_height::FCT.CFloat,texture::FP.Ptr SDLT.SDL_GPUTexture,temporary_texture::FP.Ptr SDLT.SDL_GPUTexture}|Bound_canvas {texture::FP.Ptr SDLT.SDL_GPUTexture,temporary_texture::FP.Ptr SDLT.SDL_GPUTexture}

data Album=Album {width::DW.Word32,height::DW.Word32,texture::FP.Ptr SDLT.SDL_GPUTexture}

data Timer=Off|On {timer_id::DW.Word32,interval::DW.Word64}

data Window=Window {window_id::Int,sdl_window_id::DW.Word32,sdl_window::FP.Ptr SDLT.SDL_Window,graphics_pipeline::FP.Ptr SDLT.SDL_GPUGraphicsPipeline,design_width::FCT.CFloat,design_height::FCT.CFloat,adaptive_width::FCT.CFloat,adaptive_height::FCT.CFloat,width::FCT.CFloat,height::FCT.CFloat,color::Color}

data Cursor=Cursor {visible::Bool,which::Bool,x::FCT.CFloat,start_line::Int,start_seat::Int,end_line::Int,end_seat::Int}

data Line=Line {line_break::Bool,width::FCT.CFloat,seat::DS.Seq Seat}

data Seat=Seat {char::Char,advance::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Row=Row {row_core::DS.Seq Character,index::Int,x::FCT.CFloat,y::FCT.CFloat,width::FCT.CFloat,min_down::FCT.CFloat,max_up::FCT.CFloat,min_descent::FCT.CFloat,max_ascent::FCT.CFloat}

data Character=Character {unicode::Int,font_id::Int,font_size::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,color::Color}

data Sentence=Sentence {sentence_core::DS.Seq Phrase,path::String}

data Phrase=Phrase {phrase_core::DT.Text,font_size::FCT.CFloat,color::Color}

data Clip_request=Clip_request {x::FCT.CFloat,y::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Anchor=Anchor {ratio::FCT.CFloat,offset::FCT.CFloat}

data Color=Color {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Matrix=Matrix {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat}

data Point=Point {x::FCT.CFloat,y::FCT.CFloat}

data Arrange=Arrange {point::Point,matrix::Matrix,color::Color}

data Atlas=Leaf_atlas {border::Border DW.Word32,used::Bool}|Node_atlas {border::Border DW.Word32,left_atlas::Atlas,right_atlas::Atlas}

data Glyph=Glyph {advance::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Font=Font {glyph::DIM.IntMap Glyph,descent::FCT.CFloat,ascent::FCT.CFloat}

data Atlas_font=Atlas_font {path::String,texture::FP.Ptr SDLT.SDL_GPUTexture,font_atlas::Atlas,glyph::DIM.IntMap Glyph,descent::FCT.CFloat,ascent::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,font_size::FCT.CFloat,pixel_range::FCT.CFloat,padding::DW.Word32,exponent_width::Int,exponent_height::Int,reference::Int}

data Projection_strategy=Object_strategy|Image_strategy|Image_safe_strategy

data Projection_path=Object_path {leaf_id::Int}|Image_path {leaf_id::Int}|Image_safe_path {leaf_id::Int}

data Projection_move=Object_move {leaf_id::Int,consume::Bool}|Image_move {leaf_id::Int}|Image_safe_move {leaf_id::Int}

data Insert_strategy=Min_strategy|Max_strategy|Index_strategy {seat::Int}

data Blend_factor=Blend_factor_invalid|Blend_factor_zero|Blend_factor_one|Blend_factor_constant_color|Blend_factor_dst_color|Blend_factor_src_color|Blend_factor_dst_alpha|Blend_factor_src_alpha|Blend_factor_src_alpha_saturate|Blend_factor_one_minus_constant_color|Blend_factor_one_minus_dst_color|Blend_factor_one_minus_src_color|Blend_factor_one_minus_dst_alpha|Blend_factor_one_minus_src_alpha

data Blend_op=Blend_op_invalid|Blend_op_min|Blend_op_max|Blend_op_add|Blend_op_subtract|Blend_op_reverse_subtract

data Blend_state=Blend_state {src_color_blend_factor::Blend_factor,dst_color_blend_factor::Blend_factor,color_blend_op::Blend_op,src_alpha_blend_factor::Blend_factor,dst_alpha_blend_factor::Blend_factor,alpha_blend_op::Blend_op,color_write_mask::DHS.HashSet Color_component_flag,enable_blend::Bool,enable_color_write_mask::Bool}

data Filter=Filter_nearest|Filter_linear

data Sampler_mipmap_mode=Sampler_mipmap_mode_nearest|Sampler_mipmap_mode_linear

data Sampler_address_mode=Sampler_address_mode_repeat|Sampler_address_mode_mirrored_repeat|Sampler_address_mode_clamp_to_edge

data Sampler_create_info=Sampler_create_info {min_filter::Filter,mag_filter::Filter,mipmap_mode::Sampler_mipmap_mode,address_mode_u::Sampler_address_mode,address_mode_v::Sampler_address_mode,address_mode_w::Sampler_address_mode}

data Action=Close|Resize {width::FCT.CFloat,height::FCT.CFloat}|Press {press::Press,change::Key,maintain::DHS.HashSet Key}|Click {press::Press,mouse_button::Mouse_button,x::FCT.CFloat,y::FCT.CFloat}|Move {x::FCT.CFloat,y::FCT.CFloat,delta_x::FCT.CFloat,delta_y::FCT.CFloat}|Scroll {x::FCT.CFloat,y::FCT.CFloat,delta_x::FCT.CFloat,delta_y::FCT.CFloat}

data Press=Press_down|Press_up

data Mouse_button=Mouse_button_unknown|Mouse_button_left|Mouse_button_middle|Mouse_button_right

data Submit_mode=Submit_default|Submit_canvas {canvas_id::Int}|Submit_album {album_id::Int}|Submit_atlas_font {atlas_font_id::Int} deriving Eq

data Extended=Negative_infinity|Finite {number::FCT.CFloat}|Positive_infinity deriving (Eq,Ord)

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z|Key_left|Key_down|Key_right|Key_up|Key_page_down|Key_page_up deriving (Eq,Enum)

instance DH.Hashable Key where
    hashWithSalt=key_hash_with_salt

key_hash_with_salt::ET.Has_call_stack=>Int->Key->Int
key_hash_with_salt=DH.hashUsing fromEnum

data System_cursor=System_cursor_default|System_cursor_pointer deriving (Eq,Enum)

instance DH.Hashable System_cursor where
    hashWithSalt=system_cursor_hash_with_salt

system_cursor_hash_with_salt::ET.Has_call_stack=>Int->System_cursor->Int
system_cursor_hash_with_salt=DH.hashUsing fromEnum

data Window_flag=Window_fullscreen|Window_hidden|Window_borderless|Window_resizable|Window_always_on_top deriving (Eq,Enum)

instance DH.Hashable Window_flag where
    hashWithSalt=window_flag_hash_with_salt

window_flag_hash_with_salt::ET.Has_call_stack=>Int->Window_flag->Int
window_flag_hash_with_salt=DH.hashUsing fromEnum

data Color_component_flag=Color_component_r|Color_component_g|Color_component_b|Color_component_a deriving (Eq,Enum)

instance DH.Hashable Color_component_flag where
    hashWithSalt=color_component_flag_hash_with_salt

color_component_flag_hash_with_salt::ET.Has_call_stack=>Int->Color_component_flag->Int
color_component_flag_hash_with_salt=DH.hashUsing fromEnum

data Typesetting=Typesetting {x::FCT.CFloat,y::FCT.CFloat,width::FCT.CFloat,lower::FCT.CFloat,upper::FCT.CFloat}

instance FS.Storable Typesetting where
    sizeOf=typesetting_size_of
    alignment=typesetting_alignment
    peek=typesetting_peek
    poke=typesetting_poke

typesetting_size_of::ET.Has_call_stack=>Num a=>Typesetting->a
typesetting_size_of _=20

typesetting_alignment::ET.Has_call_stack=>Num a=>Typesetting->a
typesetting_alignment _=4

typesetting_peek::ET.Has_call_stack=>FP.Ptr Typesetting->IO Typesetting
typesetting_peek ptr=do
    x<-FS.peekByteOff ptr 0
    y<-FS.peekByteOff ptr 4
    width<-FS.peekByteOff ptr 8
    lower<-FS.peekByteOff ptr 12
    upper<-FS.peekByteOff ptr 16
    return (Typesetting {x=x,y=y,width=width,lower=lower,upper=upper})

typesetting_poke::ET.Has_call_stack=>FP.Ptr Typesetting->Typesetting->IO ()
typesetting_poke ptr typesetting=case typesetting of
    Typesetting {x,y,width,lower,upper}->do
        FS.pokeByteOff ptr 0 x
        FS.pokeByteOff ptr 4 y
        FS.pokeByteOff ptr 8 width
        FS.pokeByteOff ptr 12 lower
        FS.pokeByteOff ptr 16 upper

data Clip=Clip {x::FCT.CFloat,y::FCT.CFloat,half_width::FCT.CFloat,half_height::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

instance FS.Storable Clip where
    sizeOf=clip_size_of
    alignment=clip_alignment
    peek=clip_peek
    poke=clip_poke

clip_size_of::ET.Has_call_stack=>Num a=>Clip->a
clip_size_of _=32

clip_alignment::ET.Has_call_stack=>Num a=>Clip->a
clip_alignment _=4

clip_peek::ET.Has_call_stack=>FP.Ptr Clip->IO Clip
clip_peek ptr=do
    x<-FS.peekByteOff ptr 0
    y<-FS.peekByteOff ptr 4
    half_width<-FS.peekByteOff ptr 8
    half_height<-FS.peekByteOff ptr 12
    min_u<-FS.peekByteOff ptr 16
    min_v<-FS.peekByteOff ptr 20
    max_u<-FS.peekByteOff ptr 24
    max_v<-FS.peekByteOff ptr 28
    return (Clip {x=x,y=y,half_width=half_width,half_height=half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v})

clip_poke::ET.Has_call_stack=>FP.Ptr Clip->Clip->IO ()
clip_poke ptr clip=case clip of
    Clip {x,y,half_width,half_height,min_u,min_v,max_u,max_v}->do
        FS.pokeByteOff ptr 0 x
        FS.pokeByteOff ptr 4 y
        FS.pokeByteOff ptr 8 half_width
        FS.pokeByteOff ptr 12 half_height
        FS.pokeByteOff ptr 16 min_u
        FS.pokeByteOff ptr 20 min_v
        FS.pokeByteOff ptr 24 max_u
        FS.pokeByteOff ptr 28 max_v

data Layout=Layout {address::Int,size::Int}

instance FS.Storable Layout where
    sizeOf=layout_size_of
    alignment=layout_alignment
    peek=layout_peek
    poke=layout_poke

layout_size_of::ET.Has_call_stack=>Num a=>Layout->a
layout_size_of _=16

layout_alignment::ET.Has_call_stack=>Num a=>Layout->a
layout_alignment _=8

layout_peek::ET.Has_call_stack=>FP.Ptr Layout->IO Layout
layout_peek ptr=do
    address<-FS.peekByteOff ptr 0
    size<-FS.peekByteOff ptr 8
    return (Layout {address=address,size=size})

layout_poke::ET.Has_call_stack=>FP.Ptr Layout->Layout->IO ()
layout_poke ptr layout=case layout of
    Layout {address,size}->do
        FS.pokeByteOff ptr 0 address
        FS.pokeByteOff ptr 8 size

data Parameter=Parameter {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat,border_flag::FCT.CFloat,border_left::FCT.CFloat,border_down::FCT.CFloat,border_right::FCT.CFloat,border_up::FCT.CFloat}

instance FS.Storable Parameter where
    sizeOf=parameter_size_of
    alignment=parameter_alignment
    peek=parameter_peek
    poke=parameter_poke

parameter_size_of::ET.Has_call_stack=>Num a=>Parameter->a
parameter_size_of _=48

parameter_alignment::ET.Has_call_stack=>Num a=>Parameter->a
parameter_alignment _=4

parameter_peek::ET.Has_call_stack=>FP.Ptr Parameter->IO Parameter
parameter_peek _=EF.empty_error

parameter_poke::ET.Has_call_stack=>FP.Ptr Parameter->Parameter->IO ()
parameter_poke ptr parameter=case parameter of
    Parameter {x,y,x_x,x_y,y_x,y_y,border_flag,border_left,border_down,border_right,border_up}->do
        FS.pokeByteOff ptr 0 x
        FS.pokeByteOff ptr 4 y
        FS.pokeByteOff ptr 8 x_x
        FS.pokeByteOff ptr 12 x_y
        FS.pokeByteOff ptr 16 y_x
        FS.pokeByteOff ptr 20 y_y
        FS.pokeByteOff ptr 28 border_flag
        FS.pokeByteOff ptr 32 border_left
        FS.pokeByteOff ptr 36 border_down
        FS.pokeByteOff ptr 40 border_right
        FS.pokeByteOff ptr 44 border_up

data Data=Data_bool {bool::Bool}|Data_int {int::Int}|Data_c_float {c_float::FCT.CFloat}

class Convert a b where
    convert::ET.Has_call_stack=>a->b

instance Convert Data Bool where
    convert=data_bool_convert

data_bool_convert::ET.Has_call_stack=>Data->Bool
data_bool_convert store=case store of
    Data_bool {bool}->bool
    _->EF.empty_error

instance Convert Bool Data where
    convert=bool_data_convert

bool_data_convert::ET.Has_call_stack=>Bool->Data
bool_data_convert bool=Data_bool {bool=bool}

instance Convert Data Int where
    convert=data_int_convert

data_int_convert::ET.Has_call_stack=>Data->Int
data_int_convert store=case store of
    Data_int {int}->int
    _->EF.empty_error

instance Convert Int Data where
    convert=int_data_convert

int_data_convert::ET.Has_call_stack=>Int->Data
int_data_convert int=Data_int {int=int}

instance Convert Data FCT.CFloat where
    convert=data_c_float_convert

data_c_float_convert::ET.Has_call_stack=>Data->FCT.CFloat
data_c_float_convert store=case store of
    Data_c_float {c_float}->c_float
    _->EF.empty_error

instance Convert FCT.CFloat Data where
    convert=c_float_data_convert

c_float_data_convert::ET.Has_call_stack=>FCT.CFloat->Data
c_float_data_convert c_float=Data_c_float {c_float=c_float}

class Custom a where
    type Custom_state a=b|b->a
    type Custom_event a=b|b->a
    type Custom_visual a=b|b->a
    type Custom_visual_request a=b|b->a
    type Custom_submit_data a=b|b->a
    custom_visual_collect::ET.Has_call_stack=>Arrange->FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Custom_visual a->Submit a
    custom_visual_remove::ET.Has_call_stack=>Custom_visual a->Engine a->IO (Engine a)
    custom_visual_unlock::ET.Has_call_stack=>Custom_visual a->Engine a->IO (Engine a,Custom_visual a)
    custom_visual_lock::ET.Has_call_stack=>Custom_visual a->Custom_visual a
    custom_visual_request::ET.Has_call_stack=>Custom_visual_request a->Engine a->IO (Engine a,Custom_visual a)
    custom_submit_data::ET.Has_call_stack=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->Custom_submit_data a->IO ()

{-# INLINE dynamic_bool_equal #-}
{-# INLINE dynamic_bool_and #-}
{-# INLINE dynamic_bool_or #-}
{-# INLINE dynamic_bool_xor #-}
{-# INLINE dynamic_bool_complement #-}
{-# INLINE dynamic_bool_shift #-}
{-# INLINE dynamic_bool_rotate #-}
{-# INLINE dynamic_bool_bit_size #-}
{-# INLINE dynamic_bool_bit_size_maybe #-}
{-# INLINE dynamic_bool_is_signed #-}
{-# INLINE dynamic_bool_test_bit #-}
{-# INLINE dynamic_bool_bit #-}
{-# INLINE dynamic_bool_pop_count #-}
{-# INLINE dynamic_bool_true #-}
{-# INLINE dynamic_bool_false #-}
{-# INLINE dynamic_bool_unary_operator #-}
{-# INLINE dynamic_bool_binary_operator #-}
{-# INLINE dynamic_int_addition #-}
{-# INLINE dynamic_int_multiplication #-}
{-# INLINE dynamic_int_abs #-}
{-# INLINE dynamic_int_signum #-}
{-# INLINE dynamic_int_from_integer #-}
{-# INLINE dynamic_int_negate #-}
{-# INLINE dynamic_int_unary_operator #-}
{-# INLINE dynamic_int_binary_operator #-}
{-# INLINE raw_coroutine_fmap #-}
{-# INLINE raw_coroutine_fmap_a #-}
{-# INLINE raw_coroutine_pure #-}
{-# INLINE raw_coroutine_pure_a #-}
{-# INLINE raw_coroutine_apply #-}
{-# INLINE raw_coroutine_apply_a #-}
{-# INLINE raw_coroutine_bind #-}
{-# INLINE raw_coroutine_bind_a #-}
{-# INLINE trigger_result_fmap #-}
{-# INLINE trigger_result_pure #-}
{-# INLINE trigger_result_apply #-}
{-# INLINE key_hash_with_salt #-}
{-# INLINE system_cursor_hash_with_salt #-}
{-# INLINE window_flag_hash_with_salt #-}
{-# INLINE color_component_flag_hash_with_salt #-}
{-# INLINE typesetting_size_of #-}
{-# INLINE typesetting_alignment #-}
{-# INLINE typesetting_peek #-}
{-# INLINE typesetting_poke #-}
{-# INLINE clip_size_of #-}
{-# INLINE clip_alignment #-}
{-# INLINE clip_peek #-}
{-# INLINE clip_poke #-}
{-# INLINE layout_size_of #-}
{-# INLINE layout_alignment #-}
{-# INLINE layout_peek #-}
{-# INLINE layout_poke #-}
{-# INLINE parameter_size_of #-}
{-# INLINE parameter_alignment #-}
{-# INLINE parameter_peek #-}
{-# INLINE parameter_poke #-}
{-# INLINE data_bool_convert #-}
{-# INLINE bool_data_convert #-}
{-# INLINE data_int_convert #-}
{-# INLINE int_data_convert #-}
{-# INLINE data_c_float_convert #-}
{-# INLINE c_float_data_convert #-}