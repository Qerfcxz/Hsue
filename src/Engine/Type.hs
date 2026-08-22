{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Engine.Type where

import qualified SDL.Type as SDLT
import qualified Error.Error as EE
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

newtype Dynamic_bool a b c d e=Dynamic_bool {dynamic_bool::(Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Bool}

instance Eq (Dynamic_bool a b c d e) where
    (==)=dynamic_bool_equal

dynamic_bool_equal::Dynamic_bool a b c d e->Dynamic_bool a b c d e->Bool
dynamic_bool_equal _ _=EE.quick_error "dynamic_bool_equal" 0

instance DB.Bits (Dynamic_bool a b c d e) where
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

dynamic_bool_and::Dynamic_bool a b c d e->Dynamic_bool a b c d e->Dynamic_bool a b c d e
dynamic_bool_and first_dynamic_bool second_dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_binary_operator (DB..&.) first_dynamic_bool.dynamic_bool second_dynamic_bool.dynamic_bool}

dynamic_bool_or::Dynamic_bool a b c d e->Dynamic_bool a b c d e->Dynamic_bool a b c d e
dynamic_bool_or first_dynamic_bool second_dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_binary_operator (DB..|.) first_dynamic_bool.dynamic_bool second_dynamic_bool.dynamic_bool}

dynamic_bool_xor::Dynamic_bool a b c d e->Dynamic_bool a b c d e->Dynamic_bool a b c d e
dynamic_bool_xor first_dynamic_bool second_dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_binary_operator DB.xor first_dynamic_bool.dynamic_bool second_dynamic_bool.dynamic_bool}

dynamic_bool_complement::Dynamic_bool a b c d e->Dynamic_bool a b c d e
dynamic_bool_complement dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_unary_operator DB.complement dynamic_bool.dynamic_bool}

dynamic_bool_shift::Dynamic_bool a b c d e->Int->Dynamic_bool a b c d e
dynamic_bool_shift dynamic_bool int=if int==0 then dynamic_bool else Dynamic_bool {dynamic_bool=dynamic_bool_false}

dynamic_bool_rotate::Dynamic_bool a b c d e->Int->Dynamic_bool a b c d e
dynamic_bool_rotate dynamic_bool _=dynamic_bool

dynamic_bool_bit_size::Dynamic_bool a b c d e->Int
dynamic_bool_bit_size _=1

dynamic_bool_bit_size_maybe::Dynamic_bool a b c d e->Maybe Int
dynamic_bool_bit_size_maybe _=Just 1

dynamic_bool_is_signed::Dynamic_bool a b c d e->Bool
dynamic_bool_is_signed _=False

dynamic_bool_test_bit::Dynamic_bool a b c d e->Int->Bool
dynamic_bool_test_bit _ _=EE.quick_error "dynamic_bool_test_bit" 0

dynamic_bool_bit::Int->Dynamic_bool a b c d e
dynamic_bool_bit int=if int==0 then Dynamic_bool {dynamic_bool=dynamic_bool_true} else Dynamic_bool {dynamic_bool=dynamic_bool_false}

dynamic_bool_pop_count::Dynamic_bool a b c d e->Int
dynamic_bool_pop_count _=EE.quick_error "dynamic_bool_pop_count" 0

dynamic_bool_true::(Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Bool
dynamic_bool_true _ _ _ _=True

dynamic_bool_false::(Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Bool
dynamic_bool_false _ _ _ _=False

dynamic_bool_unary_operator::(Bool->Bool)->((Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Bool)->(Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Bool
dynamic_bool_unary_operator operator dynamic_bool getter event engine widget=operator (dynamic_bool getter event engine widget)

dynamic_bool_binary_operator::(Bool->Bool->Bool)->((Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Bool)->((Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Bool)->(Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Bool
dynamic_bool_binary_operator operator first_dynamic_bool second_dynamic_bool getter event engine widget=operator (first_dynamic_bool getter event engine widget) (second_dynamic_bool getter event engine widget)

newtype Dynamic_int a b c d e=Dynamic_int {dynamic_int::(Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Int}

instance Num (Dynamic_int a b c d e) where
    (+)=dynamic_int_addition
    (*)=dynamic_int_multiplication
    abs=dynamic_int_abs
    signum=dynamic_int_signum
    fromInteger=dynamic_int_from_integer
    negate=dynamic_int_negate

dynamic_int_addition::Dynamic_int a b c d e->Dynamic_int a b c d e->Dynamic_int a b c d e
dynamic_int_addition first_dynamic_int second_dynamic_int=Dynamic_int {dynamic_int=dynamic_int_binary_operator (+) first_dynamic_int.dynamic_int second_dynamic_int.dynamic_int}

dynamic_int_multiplication::Dynamic_int a b c d e->Dynamic_int a b c d e->Dynamic_int a b c d e
dynamic_int_multiplication first_dynamic_int second_dynamic_int=Dynamic_int {dynamic_int=dynamic_int_binary_operator (*) first_dynamic_int.dynamic_int second_dynamic_int.dynamic_int}

dynamic_int_abs::Dynamic_int a b c d e->Dynamic_int a b c d e
dynamic_int_abs dynamic_int=Dynamic_int {dynamic_int=dynamic_int_unary_operator abs dynamic_int.dynamic_int}

dynamic_int_signum::Dynamic_int a b c d e->Dynamic_int a b c d e
dynamic_int_signum dynamic_int=Dynamic_int {dynamic_int=dynamic_int_unary_operator signum dynamic_int.dynamic_int}

dynamic_int_from_integer::Integer->Dynamic_int a b c d e
dynamic_int_from_integer integer=Dynamic_int {dynamic_int=const (const (const (const (fromInteger integer))))}

dynamic_int_negate::Dynamic_int a b c d e->Dynamic_int a b c d e
dynamic_int_negate dynamic_int=Dynamic_int {dynamic_int=dynamic_int_unary_operator negate dynamic_int.dynamic_int}

dynamic_int_unary_operator::(Int->Int)->((Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Int)->(Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Int
dynamic_int_unary_operator operator dynamic_int getter event engine widget=operator (dynamic_int getter event engine widget)

dynamic_int_binary_operator::(Int->Int->Int)->((Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Int)->((Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Int)->(Int->Int)->Event a->Engine b a c d e->Widget b a c d e->Int
dynamic_int_binary_operator operator first_dynamic_int second_dynamic_int getter event engine widget=operator (first_dynamic_int getter event engine widget) (second_dynamic_int getter event engine widget)

newtype Raw_coroutine a b c d e f=Raw_coroutine {iterator::Int->(Int,DS.Seq (Coroutine a b c d e),f)}

instance Functor (Raw_coroutine a b c d e) where
    fmap=raw_coroutine_fmap

raw_coroutine_fmap::(a->b)->Raw_coroutine c d e f g a->Raw_coroutine c d e f g b
raw_coroutine_fmap function raw_coroutine=Raw_coroutine {iterator=raw_coroutine_fmap_a function raw_coroutine.iterator}

raw_coroutine_fmap_a::(a->b)->(Int->(Int,DS.Seq (Coroutine c d e f g),a))->Int->(Int,DS.Seq (Coroutine c d e f g),b)
raw_coroutine_fmap_a function iterator int=let (new_int,coroutine_sequence,value)=iterator int in (new_int,coroutine_sequence,function value)

instance Applicative (Raw_coroutine a b c d e) where
    pure=raw_coroutine_pure
    (<*>)=raw_coroutine_apply

raw_coroutine_pure::a->Raw_coroutine b c d e f a
raw_coroutine_pure value=Raw_coroutine {iterator=raw_coroutine_pure_a value}

raw_coroutine_pure_a::a->Int->(Int,DS.Seq (Coroutine b c d e f),a)
raw_coroutine_pure_a value int=(int,DS.empty,value)

raw_coroutine_apply::Raw_coroutine a b c d e (f->g)->Raw_coroutine a b c d e f->Raw_coroutine a b c d e g
raw_coroutine_apply first_raw_coroutine second_raw_coroutine=Raw_coroutine {iterator=raw_coroutine_apply_a first_raw_coroutine.iterator second_raw_coroutine.iterator}

raw_coroutine_apply_a::(Int->(Int,DS.Seq (Coroutine a b c d e),f->g))->(Int->(Int,DS.Seq (Coroutine a b c d e),f))->Int->(Int,DS.Seq (Coroutine a b c d e),g)
raw_coroutine_apply_a function_iterator value_iterator int=let (function_int,function_coroutine_sequence,function)=function_iterator int in
    let (value_int,value_coroutine_sequence,value)=value_iterator function_int in (value_int,function_coroutine_sequence DS.>< value_coroutine_sequence,function value)

instance Monad (Raw_coroutine a b c d e) where
    return=pure
    (>>=)=raw_coroutine_bind

raw_coroutine_bind::Raw_coroutine a b c d e f->(f->Raw_coroutine a b c d e g)->Raw_coroutine a b c d e g
raw_coroutine_bind raw_coroutine function=Raw_coroutine {iterator=raw_coroutine_bind_a raw_coroutine.iterator function}

raw_coroutine_bind_a::(Int->(Int,DS.Seq (Coroutine a b c d e),f))->(f->Raw_coroutine a b c d e g)->Int->(Int,DS.Seq (Coroutine a b c d e),g)
raw_coroutine_bind_a iterator function int=let (new_int,coroutine_sequence,value)=iterator int in let (new_new_int,new_coroutine_sequence,new_value)=(function value).iterator new_int in (new_new_int,coroutine_sequence DS.>< new_coroutine_sequence,new_value)

data Event_result a b c d e f g=Event_result {first_value::f,update::Engine a b c d e->Engine a b c d e,second_value::g}

instance Functor (Event_result a b c d e f) where
    fmap=event_result_fmap

event_result_fmap::(a->b)->Event_result c d e f g h a->Event_result c d e f g h b
event_result_fmap function event_result=case event_result of
    Event_result {first_value,update,second_value}->Event_result {first_value=first_value,update=update,second_value=function second_value}

instance Applicative (Event_result a b c d e f) where
    pure=event_result_pure
    (<*>)=event_result_apply

event_result_pure::a->Event_result b c d e f g a
event_result_pure _=EE.quick_error "event_result_pure" 0

event_result_apply::Event_result a b c d e f (g->h)->Event_result a b c d e f g->Event_result a b c d e f h
event_result_apply _ _=EE.quick_error "event_result_apply" 0

data Engine a b c d e=Engine {custom::a,main_id::Event b->Engine a b c d e->Maybe Int,projection_strategy::Event b->Engine a b c d e->Projection_strategy,callback::FP.FunPtr (FP.Ptr ()->DW.Word32->DW.Word64->IO DW.Word64),atlas::Atlas,canvas::DIM.IntMap Canvas,album::DIM.IntMap Album,leaf::DIM.IntMap (Projection a b c d e),node::DIM.IntMap (Node a b c d e),window::DIM.IntMap Window,font::DIM.IntMap Font,atlas_font::DIM.IntMap Atlas_font,window_map::DHMS.HashMap DW.Word32 Int,font_map::DHMS.HashMap String Int,system_cursor_map::DHMS.HashMap System_cursor (FP.Ptr SDLT.SDL_Cursor),request::DS.Seq (Request a b c d e),key::DHS.HashSet Key,device::FP.Ptr SDLT.SDL_GPUDevice,texture::FP.Ptr SDLT.SDL_GPUTexture,sampler::DIM.IntMap (FP.Ptr SDLT.SDL_GPUSampler),default_sampler::FP.Ptr SDLT.SDL_GPUSampler,canvas_graphics_pipeline::FP.Ptr SDLT.SDL_GPUGraphicsPipeline,pipeline::DIM.IntMap Pipeline,shader::DIM.IntMap Shader,default_shader::FP.Ptr SDLT.SDL_GPUShader,vertex_shader::FP.Ptr SDLT.SDL_GPUShader,fragment_shader::FP.Ptr SDLT.SDL_GPUShader,vertex_buffer::FP.Ptr SDLT.SDL_GPUBuffer,index_buffer::FP.Ptr SDLT.SDL_GPUBuffer,parameter_buffer::FP.Ptr SDLT.SDL_GPUBuffer,transfer_buffer::FP.Ptr SDLT.SDL_GPUTransferBuffer,picture_transfer_buffer::FP.Ptr SDLT.SDL_GPUTransferBuffer,max_picture_size::FCT.CInt,max_vertex_size::Int,max_index_size::Int,max_parameter_size::Int,exponent_width::Int,exponent_height::Int,initial_canvas_id::Int,canvas_id::Int,initial_album_id::Int,album_id::Int,initial_font_id::Int,font_id::Int,count::Int,timer::Timer,time::DW.Word64,event_number::DW.Word32,padding::DW.Word32,u::FCT.CFloat,v::FCT.CFloat,font_size::FCT.CFloat,pixel_range::FCT.CFloat}

data Projection a b c d e=Without {ancestry_id::DS.Seq Int,object::Widget a b c d e}|With {ancestry_id::DS.Seq Int,object::Widget a b c d e,image::Widget a b c d e}

data Node a b c d e=Node {ancestry_id::DS.Seq Int,leaf_child::DIS.IntSet,node_child::DIS.IntSet,event_transform::Engine a b c d e->Event b->Event b,widget_transform::Event b->Engine a b c d e->Widget a b c d e->Widget a b c d e}

data Widget a b c d e=Group {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,index::Int,group_widget::DIM.IntMap (Widget a b c d e)}|Vector {index::Int,size::Int,vector_widget::DV.Vector (Widget a b c d e)}|Trigger {next::Event b->Engine a b c d e->Maybe Int,trigger::Event b->Engine a b c d e->Engine a b c d e}|Io_trigger {next::Event b->Engine a b c d e->Maybe Int,io_trigger::Event b->Engine a b c d e->IO (Engine a b c d e)}|Mix_trigger {next::Event b->Engine a b c d e->Maybe Int,mix_trigger::Event b->(Engine a b c d e->Engine a b c d e,Engine a b c d e->IO (Engine a b c d e)),order::Bool}|Widget_trigger {next::Event b->Engine a b c d e->Maybe Int,widget_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e),widget::Widget a b c d e}|Widget_io_trigger {next::Event b->Engine a b c d e->Maybe Int,widget_io_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->IO (Engine a b c d e)),widget::Widget a b c d e}|Widget_mix_trigger {next::Event b->Engine a b c d e->Maybe Int,widget_mix_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e,Engine a b c d e->IO (Engine a b c d e)),order::Bool,widget::Widget a b c d e}|Coroutine {index::Int,initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,variable_size::Int,user_variable_size::Int,layout_size::Int,coroutine_state::DIM.IntMap (Coroutine_state a b c d e),layout::DVS.Vector Layout,linear_coroutine::DV.Vector (Linear_coroutine a b c d e),iterative::Bool}|Store {store::Data}|Collector {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,submit::DIM.IntMap (DS.Seq Submit)}|Visual {visual::Visual}|Group_visual {arrange::Arrange,collect_order::DS.Seq Int,group_visual::DIM.IntMap Visual}|Vector_visual {arrange::Arrange,collect_order::DS.Seq Int,size::Int,vector_visual::DV.Vector Visual}|Custom_widget {custom::d}

data Widget_request a b c d e=Group_request {initial_min_index::Int,initial_max_index::Int,index::Int,insert_widget_request::DS.Seq (Insert (Widget_request a b c d e))}|Vector_request {index::Int,vector_widget_request::DS.Seq (Widget_request a b c d e)}|Trigger_request {next::Event b->Engine a b c d e->Maybe Int,trigger::Event b->Engine a b c d e->Engine a b c d e}|Io_trigger_request {next::Event b->Engine a b c d e->Maybe Int,io_trigger::Event b->Engine a b c d e->IO (Engine a b c d e)}|Mix_trigger_request {next::Event b->Engine a b c d e->Maybe Int,mix_trigger::Event b->(Engine a b c d e->Engine a b c d e,Engine a b c d e->IO (Engine a b c d e)),order::Bool}|Widget_trigger_request {next::Event b->Engine a b c d e->Maybe Int,widget_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e),widget_request::Widget_request a b c d e}|Widget_io_trigger_request {next::Event b->Engine a b c d e->Maybe Int,widget_io_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->IO (Engine a b c d e)),widget_request::Widget_request a b c d e}|Widget_mix_trigger_request {next::Event b->Engine a b c d e->Maybe Int,widget_mix_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e,Engine a b c d e->IO (Engine a b c d e)),order::Bool,widget_request::Widget_request a b c d e}|Coroutine_request {index::Int,initial_min_index::Int,initial_max_index::Int,insert_widget_request::DS.Seq (Insert (Widget_request a b c d e)),raw_coroutine::Raw_coroutine a b c d e (),iterative::Bool}|Store_request {store::Data}|Collector_request {initial_min_index::Int,initial_max_index::Int}|Visual_request {visual_request::Visual_request}|Group_visual_request {arrange::Arrange,collect_order::DS.Seq Int,group_visual_request::DIM.IntMap Visual_request}|Vector_visual_request {arrange::Arrange,collect_order::DS.Seq Int,vector_visual_request::DV.Vector Visual_request}|Custom_widget_request {custom::e}

data Request a b c d e=Reset_timer {interval::DW.Word64}|Stop_timer|Stop_timer_safe|Create_widget {leaf_id::Int,maybe_father_id::Maybe Int,widget_request::Widget_request a b c d e}|Remove_widget {leaf_id::Int}|Create_node {node_id::Int,maybe_father_id::Maybe Int,event_transform::Engine a b c d e->Event b->Event b,widget_transform::Event b->Engine a b c d e->Widget a b c d e->Widget a b c d e}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::DT.Text,window_width::FCT.CInt,window_height::FCT.CInt,color::Color,window_flag::DHS.HashSet Window_flag,blend_state::Blend_state}|Remove_window {window_id::Int}|Create_canvas {canvas_width::DW.Word32,canvas_height::DW.Word32,maybe_canvas_id::Maybe Int}|Remove_canvas {canvas_id::Int}|Create_shader {shader_id::Int,stage::DW.Word32,num_sampler::DW.Word32,num_uniform_buffer::DW.Word32,path::String}|Remove_shader {shader_id::Int}|Create_pipeline {pipeline_id::Int,maybe_vertex_shader_id::Maybe Int,fragment_shader_id::Int,blend_state::Blend_state}|Remove_pipeline {pipeline_id::Int}|Create_sampler {sampler_id::Int,sampler_create_info::Sampler_create_info}|Remove_sampler {sampler_id::Int}|Create_atlas_font {atlas_font_id::Int,exponent_width::Int,exponent_height::Int,padding::DW.Word32,width::DW.Word32,height::DW.Word32,font_size::FCT.CFloat,pixel_range::FCT.CFloat,path::String}|Remove_atlas_font {atlas_font_id::Int}|Set_window_icon {window_id::Int,path::String}|Set_window_size {window_id::Int,window_width::FCT.CInt,window_height::FCT.CInt}|Set_window_position {window_id::Int,x::FCT.CInt,y::FCT.CInt}|Set_window_title {window_id::Int,title::DT.Text}|Set_window_fullscreen {window_id::Int,fullscreen::Bool}|Set_system_cursor {system_cursor::System_cursor}|Clean_atlas|Unlock {leaf_id::Int}|Load_charset {charset::DHMS.HashMap String (DHS.HashSet Char)}|Render {window_id::Int,render_selector::Selector (),projection_move::Projection_move,maybe_sampler_id::Maybe Int}|Canvas_render {canvas_id::Int,canvas_render_selector::Selector (),projection_move::Projection_move,maybe_sampler_id::Maybe Int}|Canvas_widget_render {projection_path::Projection_path,canvas_widget_render_selector::Selector (Selector ()),projection_move::Projection_move,maybe_sampler_id::Maybe Int}|Shader_canvas {uniform::Uniform,canvas_id::Int,pipeline_id::Int,maybe_sampler_id::Maybe Int}|Io {io::Engine a b c d e->IO (Engine a b c d e)}|Custom_request {custom::c}

data Coroutine_state a b c d e=Coroutine_state {widget::Widget a b c d e,variable::DVU.Vector Int,user_variable::DVU.Vector Int,program_counter::DIM.IntMap Program_counter,index_group::DIM.IntMap (DS.Seq Int),main_index_group::DS.Seq Int,index_group_index::Int,program_counter_index::Int}

data Coroutine a b c d e=Done|Emit {emit::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)}|Wait {dynamic_int::Dynamic_int a b c d e}|Forever {coroutine::Coroutine a b c d e}|Then {coroutine_sequence::DS.Seq (Coroutine a b c d e)}|While {dynamic_bool::Dynamic_bool a b c d e,coroutine::Coroutine a b c d e}|Pause {dynamic_bool::Dynamic_bool a b c d e,coroutine::Coroutine a b c d e}|Skip {dynamic_bool::Dynamic_bool a b c d e,coroutine::Coroutine a b c d e}|Assign {dynamic_int::Dynamic_int a b c d e,int::Int}|Repeat {dynamic_int::Dynamic_int a b c d e,coroutine::Coroutine a b c d e}|Clone {int::Int,coroutine::Coroutine a b c d e}|If {dynamic_bool::Dynamic_bool a b c d e,first_coroutine::Coroutine a b c d e,second_coroutine::Coroutine a b c d e}|Dynamic_clone {dynamic_int::Dynamic_int a b c d e,int::Int,coroutine::Coroutine a b c d e}|Case {dynamic_int::Dynamic_int a b c d e,int::Int,coroutine_sequence::DS.Seq (Coroutine a b c d e)}|Fork {int::Int,coroutine::Coroutine a b c d e,coroutine_sequence::DS.Seq (Coroutine a b c d e)}|Race {dynamic_int::Dynamic_int a b c d e,first_int::Int,second_int::Int,coroutine_sequence::DS.Seq (Coroutine a b c d e)}

data Linear_coroutine a b c d e=Linear_end|Linear_emit {emit::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)}|Linear_wait {int_index::Int}|Linear_countdown {int_index::Int}|Linear_wake {int_index::Int}|Linear_fork {code_index::Int}|Linear_yield {code_index::Int}|Linear_jump {code_index::Int}|Linear_kill {group_int_index::DIM.IntMap Int}|Linear_one_less_jump {int_index::Int,code_index::Int}|Linear_one_more_jump {int_index::Int,code_index::Int}|Linear_kill_clone {int_index::Int,clone_number::Int}|Linear_dynamic_int {int_index::Int,dynamic_int::Dynamic_int a b c d e}|Linear_true_jump {code_index::Int,dynamic_bool::Dynamic_bool a b c d e}|Linear_false_jump {code_index::Int,dynamic_bool::Dynamic_bool a b c d e}|Linear_less_jump {int_index::Int,code_index::Int,int::Int}|Linear_clone {int_index::Int,clone_number::Int,int::Int}|Linear_wake_group {int_index::Int,dynamic_int::Dynamic_int a b c d e,int::Int}|Linear_assign {user_int_index::Int,clone_number::Int,dynamic_int::Dynamic_int a b c d e}|Linear_create_group {first_int_index::Int,second_int_index::Int,group_code_index::DIM.IntMap Int,int::Int}|Linear_create_active_group {first_int_index::Int,second_int_index::Int,group_code_index::DIM.IntMap Int,int::Int}|Linear_dynamic_clone {int_index::Int,code_index::Int,clone_number::Int,dynamic_int::Dynamic_int a b c d e,int::Int}

data Event a=Empty|Quit|Time {tick::Int,time::DW.Word64,interval::DW.Word64}|At {window_id::Int,action::Action}|Custom_event {custom::a}

data Selector a=None_selector|Combine_selector {combine_selector::DS.Seq (Selector a)}|Self_selector {value::a}|All_selector {maybe_value::Maybe a,value::a}|Trigger_selector {maybe_value::Maybe a,value::a,bounded::Bool}|Default_selector {maybe_value::Maybe a,value::a,bounded::Bool}|Hosted_selector {maybe_value::Maybe a,selector::Selector a,bounded::Bool,strict::Bool}|Any_selector {maybe_value::Maybe a,selector::Selector a,strict::Bool}|Group_selector {maybe_value::Maybe a,group_selector::DIM.IntMap (Selector a),bounded::Bool,strict::Bool}|Vector_selector {maybe_value::Maybe a,vector_selector::DIM.IntMap (Selector a),bounded::Bool,strict::Bool}|Widget_trigger_selector {maybe_value::Maybe a,selector::Selector a,strict::Bool}|Widget_io_trigger_selector {maybe_value::Maybe a,selector::Selector a,strict::Bool}|Widget_mix_trigger_selector {maybe_value::Maybe a,selector::Selector a,strict::Bool}|Coroutine_selector {maybe_value::Maybe a,coroutine_selector::DIM.IntMap (Selector a),bounded::Bool,strict::Bool}

data Insert a=Insert {insert_strategy::Insert_strategy,value::a}

data Border a=Border {left::a,down::a,right::a,up::a}

data Submit=Submit {submit_mode::Submit_mode,vertex::DS.Seq Vertex,index::DS.Seq DW.Word32,parameter::Parameter,vertex_size::DW.Word32,index_size::DW.Word32}

data Uniform=Uniform {size::Int,alignment::Int,write::FP.Ptr ()->IO ()}

data Shader=Shader {sdl_shader::FP.Ptr SDLT.SDL_GPUShader,reference::Int}

data Pipeline=Pipeline {sdl_pipeline::FP.Ptr SDLT.SDL_GPUGraphicsPipeline,vertex_shader_id::Int,fragment_shader_id::Int}|Default_pipeline {sdl_pipeline::FP.Ptr SDLT.SDL_GPUGraphicsPipeline,fragment_shader_id::Int}

data Program_counter=Program_counter {code_index::Int,clone_index::Int}

data Canvas=Free_canvas {width::DW.Word32,height::DW.Word32,half_width::FCT.CFloat,half_height::FCT.CFloat,texture::FP.Ptr SDLT.SDL_GPUTexture,temporary_texture::FP.Ptr SDLT.SDL_GPUTexture}|Bound_canvas {texture::FP.Ptr SDLT.SDL_GPUTexture,temporary_texture::FP.Ptr SDLT.SDL_GPUTexture}

data Album=Album {width::DW.Word32,height::DW.Word32,texture::FP.Ptr SDLT.SDL_GPUTexture}

data Timer=Off|On {timer_id::DW.Word32,interval::DW.Word64}

data Window=Window {window_id::Int,sdl_window_id::DW.Word32,sdl_window::FP.Ptr SDLT.SDL_Window,graphics_pipeline::FP.Ptr SDLT.SDL_GPUGraphicsPipeline,design_width::FCT.CFloat,design_height::FCT.CFloat,adaptive_width::FCT.CFloat,adaptive_height::FCT.CFloat,width::FCT.CFloat,height::FCT.CFloat,color::Color}

data Cursor=Cursor {visible::Bool,which::Bool,x::FCT.CFloat,start_multiline::Int,start_line::Int,start_element::Int,end_multiline::Int,end_line::Int,end_element::Int}

data Multiline=Multiline {multiline_core::DS.Seq Line,size::Int}

data Line=Line {line_core::DS.Seq Element,x::FCT.CFloat,y::FCT.CFloat,width::FCT.CFloat,lower::FCT.CFloat,upper::FCT.CFloat}

data Element=Element {unicode::Int,advance::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Row=Blank|Row {row_core::DS.Seq Character,x::FCT.CFloat,y::FCT.CFloat,width::FCT.CFloat,min_down::FCT.CFloat,max_up::FCT.CFloat,min_descent::FCT.CFloat,max_ascent::FCT.CFloat}

data Character=Character {unicode::Int,font_id::Int,font_size::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,color::Color}

data Sentence=Sentence {sentence_core::DS.Seq Phrase,path::String}

data Phrase=Phrase {phrase_core::DT.Text,font_size::FCT.CFloat,color::Color}

data Visual=Rectangle {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat}|Triangle {arrange::Arrange,first_point::Point,second_point::Point,third_point::Point}|Convex_polygon {arrange::Arrange,point_set::DS.Seq Point}|Regular_polygon {arrange::Arrange,number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,path::String,locked::Bool}|Large_picture {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat,album_id::Int}|Atlas {arrange::Arrange,clip_request::DS.Seq Clip_request,path::String,clip::DVS.Vector Clip,size::Int,index::Int,locked::Bool}|Large_atlas {arrange::Arrange,clip::DVS.Vector Clip,size::Int,album_id::Int,index::Int}|Animation {arrange::Arrange,delay::DVS.Vector FCT.CFloat,moment::FCT.CFloat,half_width::FCT.CFloat,half_height::FCT.CFloat,reciprocal_width::FCT.CFloat,reciprocal_height::FCT.CFloat,padding::FCT.CFloat,width_number::Int,height_number::Int,album_number::Int,album_id::Int,count::Int,index::Int}|Text {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat,current_y::FCT.CFloat,min_y::FCT.CFloat,max_y::FCT.CFloat,article::DS.Seq (DS.Seq Row),charset::DHMS.HashMap String (DHS.HashSet Char),locked::Bool}|Editor {arrange::Arrange,half_width::FCT.CFloat,half_height::FCT.CFloat,cursor_width::FCT.CFloat,current_y::FCT.CFloat,min_y::FCT.CFloat,max_y::FCT.CFloat,font_size::FCT.CFloat,atlas_font_id::Int,text_color::Color,cursor_color::Color,box_color::Color,selected_color::Color,line_width::Int->FCT.CFloat,line_typesetting::Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat),cursor::Cursor,multiline::DS.Seq Multiline}|Canvas {arrange::Arrange,canvas_width::DW.Word32,canvas_height::DW.Word32,half_width::FCT.CFloat,half_height::FCT.CFloat,canvas_id::Int}

data Visual_request=Rectangle_request {arrange::Arrange,rectangle_width::FCT.CFloat,rectangle_height::FCT.CFloat}|Triangle_request {arrange::Arrange,first_point::Point,second_point::Point,third_point::Point}|Convex_polygon_request {arrange::Arrange,point_set::DS.Seq Point}|Regular_polygon_request {arrange::Arrange,number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture_request {arrange::Arrange,path::String}|Large_picture_request {arrange::Arrange,path::String}|Atlas_request {arrange::Arrange,clip_request::DS.Seq Clip_request,path::String}|Large_atlas_request {arrange::Arrange,clip_request::DS.Seq Clip_request,path::String}|Animation_request {arrange::Arrange,min_delay::FCT.CFloat,animation_width::DW.Word32,animation_height::DW.Word32,padding::Int,path::String}|Text_request {arrange::Arrange,text_width::FCT.CFloat,text_height::FCT.CFloat,article::DS.Seq (DS.Seq Sentence),calculate_width::DS.Seq Row->DS.Seq (DS.Seq Row)->Int->FCT.CFloat,calculate_typesetting::DS.Seq (DS.Seq Row)->Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat),load::Bool}|Editor_request {arrange::Arrange,editor_width::FCT.CFloat,editor_height::FCT.CFloat,cursor_width::FCT.CFloat,font_size::FCT.CFloat,atlas_font_id::Int,text_color::Color,cursor_color::Color,box_color::Color,selected_color::Color,line_width::Int->FCT.CFloat,line_typesetting::Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat)}|Canvas_request {arrange::Arrange,canvas_width::DW.Word32,canvas_height::DW.Word32,maybe_canvas_id::Maybe Int}

data Clip_request=Clip_request {x::FCT.CFloat,y::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Color=Color {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Matrix=Matrix {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat}

data Point=Point {x::FCT.CFloat,y::FCT.CFloat}

data Arrange=Arrange {point::Point,matrix::Matrix,color::Color}

data Atlas=Leaf_atlas {border::Border DW.Word32,used::Bool}|Node_atlas {border::Border DW.Word32,left_atlas::Atlas,right_atlas::Atlas}

data Glyph=Glyph {advance::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Font=Font {descent::FCT.CFloat,ascent::FCT.CFloat,glyph::DIM.IntMap Glyph}

data Atlas_font=Atlas_font {font_atlas::Atlas,texture::FP.Ptr SDLT.SDL_GPUTexture,exponent_width::Int,exponent_height::Int,padding::DW.Word32,u::FCT.CFloat,v::FCT.CFloat,font_size::FCT.CFloat,pixel_range::FCT.CFloat,descent::FCT.CFloat,ascent::FCT.CFloat,glyph::DIM.IntMap Glyph,path::String,reference::Int}

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

data Press=Press_up|Press_down

data Mouse_button=Mouse_button_unknown|Mouse_button_left|Mouse_button_middle|Mouse_button_right

data Submit_mode=Submit_default|Submit_canvas {canvas_id::Int}|Submit_album {album_id::Int}|Submit_atlas_font {atlas_font_id::Int} deriving Eq

data Extended=Negative_infinity|Finite {number::FCT.CFloat}|Positive_infinity deriving (Eq,Ord)

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z|Key_left|Key_down|Key_right|Key_up|Key_page_down|Key_page_up deriving (Eq,Enum)

instance DH.Hashable Key where
    hashWithSalt=key_hashWithSalt

key_hashWithSalt::Int->Key->Int
key_hashWithSalt=DH.hashUsing fromEnum

data System_cursor=System_cursor_default|System_cursor_pointer deriving (Eq,Enum)

instance DH.Hashable System_cursor where
    hashWithSalt=system_cursor_hashWithSalt

system_cursor_hashWithSalt::Int->System_cursor->Int
system_cursor_hashWithSalt=DH.hashUsing fromEnum

data Window_flag=Window_fullscreen|Window_hidden|Window_borderless|Window_resizable|Window_always_on_top deriving (Eq,Enum)

instance DH.Hashable Window_flag where
    hashWithSalt=window_flag_hashWithSalt

window_flag_hashWithSalt::Int->Window_flag->Int
window_flag_hashWithSalt=DH.hashUsing fromEnum

data Color_component_flag=Color_component_r|Color_component_g|Color_component_b|Color_component_a deriving (Eq,Enum)

instance DH.Hashable Color_component_flag where
    hashWithSalt=color_component_flag_hashWithSalt

color_component_flag_hashWithSalt::Int->Color_component_flag->Int
color_component_flag_hashWithSalt=DH.hashUsing fromEnum

data Clip=Clip {x::FCT.CFloat,y::FCT.CFloat,half_width::FCT.CFloat,half_height::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

instance FS.Storable Clip where
    sizeOf=clip_size_of
    alignment=clip_alignment
    peek=clip_peek
    poke=clip_poke

clip_size_of::Num a=>Clip->a
clip_size_of _=32

clip_alignment::Num a=>Clip->a
clip_alignment _=4

clip_peek::FP.Ptr Clip->IO Clip
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

clip_poke::FP.Ptr Clip->Clip->IO ()
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

layout_size_of::Num a=>Layout->a
layout_size_of _=16

layout_alignment::Num a=>Layout->a
layout_alignment _=8

layout_peek::FP.Ptr Layout->IO Layout
layout_peek ptr=do
    address<-FS.peekByteOff ptr 0
    size<-FS.peekByteOff ptr 8
    return (Layout {address=address,size=size})

layout_poke::FP.Ptr Layout->Layout->IO ()
layout_poke ptr layout=case layout of
    Layout {address,size}->do
        FS.pokeByteOff ptr 0 address
        FS.pokeByteOff ptr 8 size

data Vertex=Vertex {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,parameter_id::FCT.CFloat,font_size::FCT.CFloat}

instance FS.Storable Vertex where
    sizeOf=vertex_size_of
    alignment=vertex_alignment
    peek=vertex_peek
    poke=vertex_poke

vertex_size_of::Num a=>Vertex->a
vertex_size_of _=40

vertex_alignment::Num a=>Vertex->a
vertex_alignment _=4

vertex_peek::FP.Ptr Vertex->IO Vertex
vertex_peek _=EE.quick_error "vertex_peek" 0

vertex_poke::FP.Ptr Vertex->Vertex->IO ()
vertex_poke ptr vertex=case vertex of
    Vertex {red,green,blue,alpha,x,y,u,v,parameter_id,font_size}->do
        FS.pokeByteOff ptr 0 red
        FS.pokeByteOff ptr 4 green
        FS.pokeByteOff ptr 8 blue
        FS.pokeByteOff ptr 12 alpha
        FS.pokeByteOff ptr 16 x
        FS.pokeByteOff ptr 20 y
        FS.pokeByteOff ptr 24 u
        FS.pokeByteOff ptr 28 v
        FS.pokeByteOff ptr 32 parameter_id
        FS.pokeByteOff ptr 36 font_size

data Parameter=Parameter {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat,border_flag::FCT.CFloat,border_left::FCT.CFloat,border_down::FCT.CFloat,border_right::FCT.CFloat,border_up::FCT.CFloat}

instance FS.Storable Parameter where
    sizeOf=parameter_size_of
    alignment=parameter_alignment
    peek=parameter_peek
    poke=parameter_poke

parameter_size_of::Num a=>Parameter->a
parameter_size_of _=48

parameter_alignment::Num a=>Parameter->a
parameter_alignment _=4

parameter_peek::FP.Ptr Parameter->IO Parameter
parameter_peek _=EE.quick_error "parameter_peek" 0

parameter_poke::FP.Ptr Parameter->Parameter->IO ()
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
    convert::a->b

instance Convert Data Bool where
    convert=data_bool_convert

data_bool_convert::Data->Bool
data_bool_convert store=case store of
    Data_bool {bool}->bool
    _->EE.quick_error "data_bool_convert" 0

instance Convert Bool Data where
    convert=bool_data_convert

bool_data_convert::Bool->Data
bool_data_convert bool=Data_bool {bool=bool}

instance Convert Data Int where
    convert=data_int_convert

data_int_convert::Data->Int
data_int_convert store=case store of
    Data_int {int}->int
    _->EE.quick_error "data_int_convert" 0

instance Convert Int Data where
    convert=int_data_convert

int_data_convert::Int->Data
int_data_convert int=Data_int {int=int}

instance Convert Data FCT.CFloat where
    convert=data_c_float_convert

data_c_float_convert::Data->FCT.CFloat
data_c_float_convert store=case store of
    Data_c_float {c_float}->c_float
    _->EE.quick_error "data_c_float_convert" 0

instance Convert FCT.CFloat Data where
    convert=c_float_data_convert

c_float_data_convert::FCT.CFloat->Data
c_float_data_convert c_float=Data_c_float {c_float=c_float}

class Custom_request a where
    custom_request::a->Engine b c a d e->IO (Engine b c a d e)

class Custom_widget a where
    custom_widget_run::Event b->Engine c b d a e->a->(a,Engine c b d a e->Engine c b d a e,Event b->Engine c b d a e->Maybe Int)
    custom_widget_collect::FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->a->DS.Seq Submit
    custom_widget_remove::a->Engine b c d a e->IO (Engine b c d a e)
    custom_widget_lock::a->a
    custom_widget_unlock::a->Engine b c d a e->IO (Engine b c d a e,a)

class Custom_widget_request a where
    custom_widget_request::a->Engine b c d e a->IO (Engine b c d e a,e)

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
{-# INLINE event_result_fmap #-}
{-# INLINE event_result_pure #-}
{-# INLINE event_result_apply #-}
{-# INLINE key_hashWithSalt #-}
{-# INLINE system_cursor_hashWithSalt #-}
{-# INLINE window_flag_hashWithSalt #-}
{-# INLINE color_component_flag_hashWithSalt #-}
{-# INLINE clip_size_of #-}
{-# INLINE clip_alignment #-}
{-# INLINE clip_peek #-}
{-# INLINE clip_poke #-}
{-# INLINE layout_size_of #-}
{-# INLINE layout_alignment #-}
{-# INLINE layout_peek #-}
{-# INLINE layout_poke #-}
{-# INLINE vertex_size_of #-}
{-# INLINE vertex_alignment #-}
{-# INLINE vertex_peek #-}
{-# INLINE vertex_poke #-}
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