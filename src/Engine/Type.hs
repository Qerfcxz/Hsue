{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Engine.Type where

import qualified SDL.Type as SDLT
import qualified Error.Error as EE
import qualified Data.Bits as DB
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Data.Vector as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

newtype Raw_coroutine a b c d e f=Raw_coroutine {iterator::Int->(Int,DSeq.Seq (Coroutine a b c d e),f)}

instance Functor (Raw_coroutine a b c d e) where
    fmap=raw_coroutine_fmap

raw_coroutine_fmap::(a->b)->Raw_coroutine c d e f g a->Raw_coroutine c d e f g b
raw_coroutine_fmap function raw_coroutine=Raw_coroutine {iterator=raw_coroutine_fmap_a function raw_coroutine.iterator}

raw_coroutine_fmap_a::(a->b)->(Int->(Int,DSeq.Seq (Coroutine c d e f g),a))->Int->(Int,DSeq.Seq (Coroutine c d e f g),b)
raw_coroutine_fmap_a function iterator int=let (new_int,coroutine_sequence,value)=iterator int in (new_int,coroutine_sequence,function value)

instance Applicative (Raw_coroutine a b c d e) where
    pure=raw_coroutine_pure
    (<*>)=raw_coroutine_apply

raw_coroutine_pure::a->Raw_coroutine b c d e f a
raw_coroutine_pure value=Raw_coroutine {iterator=raw_coroutine_pure_a value}

raw_coroutine_pure_a::a->Int->(Int,DSeq.Seq (Coroutine b c d e f),a)
raw_coroutine_pure_a value int=(int,DSeq.empty,value)

raw_coroutine_apply::Raw_coroutine a b c d e (f->g)->Raw_coroutine a b c d e f->Raw_coroutine a b c d e g
raw_coroutine_apply first_raw_coroutine second_raw_coroutine=Raw_coroutine {iterator=raw_coroutine_apply_a first_raw_coroutine.iterator second_raw_coroutine.iterator}

raw_coroutine_apply_a::(Int->(Int,DSeq.Seq (Coroutine a b c d e),f->g))->(Int->(Int,DSeq.Seq (Coroutine a b c d e),f))->Int->(Int,DSeq.Seq (Coroutine a b c d e),g)
raw_coroutine_apply_a function_iterator value_iterator int=let (function_int,function_coroutine_sequence,function)=function_iterator int in
    let (value_int,value_coroutine_sequence,value)=value_iterator function_int in (value_int,function_coroutine_sequence DSeq.>< value_coroutine_sequence,function value)

instance Monad (Raw_coroutine a b c d e) where
    return=pure
    (>>=)=raw_coroutine_bind

raw_coroutine_bind::Raw_coroutine a b c d e f->(f->Raw_coroutine a b c d e g)->Raw_coroutine a b c d e g
raw_coroutine_bind raw_coroutine function=Raw_coroutine {iterator=raw_coroutine_bind_a raw_coroutine.iterator function}

raw_coroutine_bind_a::(Int->(Int,DSeq.Seq (Coroutine a b c d e),f))->(f->Raw_coroutine a b c d e g)->Int->(Int,DSeq.Seq (Coroutine a b c d e),g)
raw_coroutine_bind_a iterator function int=let (new_int,coroutine_sequence,value)=iterator int in let (new_new_int,new_coroutine_sequence,new_value)=(function value).iterator new_int in (new_new_int,coroutine_sequence DSeq.>< new_coroutine_sequence,new_value)

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

dynamic_bool_true::(Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Bool
dynamic_bool_true _ _ _ _=True

dynamic_bool_false::(Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Bool
dynamic_bool_false _ _ _ _=False

dynamic_bool_unary_operator::(Bool->Bool)->((Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Bool)->(Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Bool
dynamic_bool_unary_operator operator dynamic_bool getter event engine widget=operator (dynamic_bool getter event engine widget)

dynamic_bool_binary_operator::(Bool->Bool->Bool)->((Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Bool)->((Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Bool)->(Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Bool
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

dynamic_int_unary_operator::(Int->Int)->((Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Int)->(Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Int
dynamic_int_unary_operator operator dynamic_int getter event engine widget=operator (dynamic_int getter event engine widget)

dynamic_int_binary_operator::(Int->Int->Int)->((Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Int)->((Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Int)->(Int->Int)->Event b->Engine a b c d e->Widget a b c d e->Int
dynamic_int_binary_operator operator first_dynamic_int second_dynamic_int getter event engine widget=operator (first_dynamic_int getter event engine widget) (second_dynamic_int getter event engine widget)

data Engine a b c d e=Engine {custom::a,main_id::Event b->Engine a b c d e->Maybe Int,projection_strategy::Event b->Engine a b c d e->Projection_strategy,callback::FP.FunPtr (FP.Ptr ()->DW.Word32->DW.Word64->IO DW.Word64),atlas::Atlas,album::DIM.IntMap Album,leaf::DIM.IntMap (Projection a b c d e),node::DIM.IntMap (Node a b c d e),window::DIM.IntMap Window,font::DIM.IntMap Font,window_map::DM.Map DW.Word32 Int,font_map::DM.Map String Int,request::DSeq.Seq (Request a b c d e),key::DSet.Set Key,device::FP.Ptr SDLT.SDL_GPUDevice,texture::FP.Ptr SDLT.SDL_GPUTexture,sampler::FP.Ptr SDLT.SDL_GPUSampler,vertex_shader::FP.Ptr SDLT.SDL_GPUShader,fragment_shader::FP.Ptr SDLT.SDL_GPUShader,vertex_buffer::FP.Ptr SDLT.SDL_GPUBuffer,index_buffer::FP.Ptr SDLT.SDL_GPUBuffer,parameter_buffer::FP.Ptr SDLT.SDL_GPUBuffer,transfer_buffer::FP.Ptr SDLT.SDL_GPUTransferBuffer,picture_transfer_buffer::FP.Ptr SDLT.SDL_GPUTransferBuffer,picture_size::FCT.CInt,vertex_size::Int,index_size::Int,parameter_size::Int,initial_album_id::Int,album_id::Int,initial_font_id::Int,font_id::Int,count::Int,timer::Timer,time::DW.Word64,event_number::DW.Word32,padding::DW.Word32,width::DW.Word32,height::DW.Word32,reciprocal_width::FCT.CFloat,reciprocal_height::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,font_size::FCT.CFloat,pixel_range::FCT.CFloat}

data Projection a b c d e=Without {ancestry_id::DSeq.Seq Int,object::Widget a b c d e}|With {ancestry_id::DSeq.Seq Int,object::Widget a b c d e,image::Widget a b c d e}

data Node a b c d e=Node {ancestry_id::DSeq.Seq Int,leaf_child::DIS.IntSet,node_child::DIS.IntSet,event_transform::Engine a b c d e->Event b->Event b,widget_transform::Event b->Engine a b c d e->Widget a b c d e->Widget a b c d e}

data Widget a b c d e=Group {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,index::Int,group_widget::DIM.IntMap (Widget a b c d e)}|Vector {index::Int,vector_widget::DV.Vector (Widget a b c d e)}|Trigger {next::Event b->Engine a b c d e->Maybe Int,trigger::Event b->Engine a b c d e->Engine a b c d e}|Io_trigger {next::Event b->Engine a b c d e->Maybe Int,io_trigger::Event b->Engine a b c d e->IO (Engine a b c d e)}|Mix_trigger {next::Event b->Engine a b c d e->Maybe Int,mix_trigger::Event b->(Engine a b c d e->Engine a b c d e,Engine a b c d e->IO (Engine a b c d e)),order::Bool}|Widget_trigger {next::Event b->Engine a b c d e->Maybe Int,widget_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e),widget::Widget a b c d e}|Widget_io_trigger {next::Event b->Engine a b c d e->Maybe Int,widget_io_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->IO (Engine a b c d e)),widget::Widget a b c d e}|Widget_mix_trigger {next::Event b->Engine a b c d e->Maybe Int,widget_mix_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e,Engine a b c d e->IO (Engine a b c d e)),order::Bool,widget::Widget a b c d e}|Coroutine {index::Int,initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,variable_length::Int,user_variable_length::Int,coroutine_state::DIM.IntMap (Coroutine_state a b c d e),layout::DVS.Vector Layout,linear_coroutine::DV.Vector (Linear_coroutine a b c d e),iterative::Bool}|Store {store::Data}|Collector {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,submit::DIM.IntMap (DSeq.Seq Submit)}|Visual {origin::Point,matrix::Matrix,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,visual::Visual}|Text {origin::Point,matrix::Matrix,width::FCT.CFloat,height::FCT.CFloat,y::FCT.CFloat,max_y::FCT.CFloat,article::DSeq.Seq (DSeq.Seq Row),charset::DM.Map String (DSet.Set Char),locked::Bool}|Custom_widget {custom::d}

data Request a b c d e=Reset_timer {interval::DW.Word64}|Stop_timer|Stop_timer_safe|Create_widget {leaf_id::Int,maybe_father_id::Maybe Int,widget_request::Widget_request a b c d e}|Remove_widget {leaf_id::Int}|Create_node {node_id::Int,maybe_father_id::Maybe Int,event_transform::Engine a b c d e->Event b->Event b,widget_transform::Event b->Engine a b c d e->Widget a b c d e->Widget a b c d e}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::DT.Text,width::FCT.CInt,height::FCT.CInt,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,window_flag::DSet.Set Window_flag}|Remove_window {window_id::Int}|Clean_atlas|Unlock {leaf_id::Int}|Load_charset {charset::DM.Map String (DSet.Set Char)}|Render {window_id::Int,projection_move::Projection_move}|Io {io::Engine a b c d e->IO (Engine a b c d e)}|Custom_request {custom::c}

data Widget_request a b c d e=Group_request {initial_min_index::Int,initial_max_index::Int,index::Int,insert_widget_request::DSeq.Seq (Insert (Widget_request a b c d e))}|Vector_request {index::Int,vector_widget_request::DSeq.Seq (Widget_request a b c d e)}|Trigger_request {next::Event b->Engine a b c d e->Maybe Int,trigger::Event b->Engine a b c d e->Engine a b c d e}|Io_trigger_request {next::Event b->Engine a b c d e->Maybe Int,io_trigger::Event b->Engine a b c d e->IO (Engine a b c d e)}|Mix_trigger_request {next::Event b->Engine a b c d e->Maybe Int,mix_trigger::Event b->(Engine a b c d e->Engine a b c d e,Engine a b c d e->IO (Engine a b c d e)),order::Bool}|Widget_trigger_request {next::Event b->Engine a b c d e->Maybe Int,widget_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e),widget_request::Widget_request a b c d e}|Widget_io_trigger_request {next::Event b->Engine a b c d e->Maybe Int,widget_io_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->IO (Engine a b c d e)),widget_request::Widget_request a b c d e}|Widget_mix_trigger_request {next::Event b->Engine a b c d e->Maybe Int,widget_mix_trigger::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e,Engine a b c d e->IO (Engine a b c d e)),order::Bool,widget_request::Widget_request a b c d e}|Coroutine_request {index::Int,initial_min_index::Int,initial_max_index::Int,insert_widget_request::DSeq.Seq (Insert (Widget_request a b c d e)),raw_coroutine::Raw_coroutine a b c d e (),iterative::Bool}|Store_request {store::Data}|Collector_request {initial_min_index::Int,initial_max_index::Int}|Visual_request {origin::Point,matrix::Matrix,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,visual_request::Visual_request}|Text_request {origin::Point,matrix::Matrix,width::FCT.CFloat,height::FCT.CFloat,article::DSeq.Seq (DSeq.Seq Sentence),calculate_width::Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat,calculate_typesetting::Int->DSeq.Seq (DSeq.Seq Row)->(FCT.CFloat,FCT.CFloat),load::Bool}|Custom_widget_request {custom::e}

data Coroutine_state a b c d e=Coroutine_state {widget::Widget a b c d e,variable::DVU.Vector Int,user_variable::DVU.Vector Int,program_counter::DIM.IntMap Program_counter,index_group::DIM.IntMap (DSeq.Seq Int),main_index_group::DSeq.Seq Int,index_group_index::Int,program_counter_index::Int}

data Coroutine a b c d e=Done|Emit {emit::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)}|Wait {dynamic_int::Dynamic_int a b c d e}|Forever {coroutine::Coroutine a b c d e}|Then {coroutine_sequence::DSeq.Seq (Coroutine a b c d e)}|While {dynamic_bool::Dynamic_bool a b c d e,coroutine::Coroutine a b c d e}|Pause {dynamic_bool::Dynamic_bool a b c d e,coroutine::Coroutine a b c d e}|Skip {dynamic_bool::Dynamic_bool a b c d e,coroutine::Coroutine a b c d e}|Assign {dynamic_int::Dynamic_int a b c d e,int::Int}|Repeat {dynamic_int::Dynamic_int a b c d e,coroutine::Coroutine a b c d e}|Clone {int::Int,coroutine::Coroutine a b c d e}|If {dynamic_bool::Dynamic_bool a b c d e,first_coroutine::Coroutine a b c d e,second_coroutine::Coroutine a b c d e}|Dynamic_clone {dynamic_int::Dynamic_int a b c d e,int::Int,coroutine::Coroutine a b c d e}|Case {dynamic_int::Dynamic_int a b c d e,int::Int,coroutine_sequence::DSeq.Seq (Coroutine a b c d e)}|Fork {int::Int,coroutine::Coroutine a b c d e,coroutine_sequence::DSeq.Seq (Coroutine a b c d e)}|Race {dynamic_int::Dynamic_int a b c d e,first_int::Int,second_int::Int,coroutine_sequence::DSeq.Seq (Coroutine a b c d e)}

data Linear_coroutine a b c d e=Linear_end|Linear_emit {emit::Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)}|Linear_wait {int_index::Int}|Linear_kill_fork {int_index::Int}|Linear_countdown {int_index::Int}|Linear_wake {int_index::Int}|Linear_fork {code_index::Int}|Linear_yield {code_index::Int}|Linear_jump {code_index::Int}|Linear_one_less_jump {int_index::Int,code_index::Int}|Linear_one_more_jump {int_index::Int,code_index::Int}|Linear_kill_clone {int_index::Int,clone_number::Int}|Linear_dynamic_int {int_index::Int,dynamic_int::Dynamic_int a b c d e}|Linear_int {int_index::Int,int::Int}|Linear_kill_group {int_index::Int,int::Int}|Linear_true_jump {code_index::Int,dynamic_bool::Dynamic_bool a b c d e}|Linear_false_jump {code_index::Int,dynamic_bool::Dynamic_bool a b c d e}|Linear_less_jump {int_index::Int,code_index::Int,int::Int}|Linear_clone {int_index::Int,clone_number::Int,int::Int}|Linear_wake_group {int_index::Int,dynamic_int::Dynamic_int a b c d e,int::Int}|Linear_assign {user_int_index::Int,clone_number::Int,dynamic_int::Dynamic_int a b c d e}|Linear_create_group {first_int_index::Int,second_int_index::Int,group_code_index::DIM.IntMap Int,int::Int}|Linear_dynamic_clone {int_index::Int,code_index::Int,clone_number::Int,dynamic_int::Dynamic_int a b c d e,int::Int}

data Event a=Empty|Quit|Time {tick::Int,time::DW.Word64,interval::DW.Word64}|At {window_id::Int,action::Action}|Custom_event {custom::a}

data Selector a=None_selector|Combine_selector {combine_selector::DSeq.Seq (Selector a)}|Self_selector {value::a}|All_selector {value::a,maybe_value::Maybe a}|Abstain_selector {value::a,maybe_value::Maybe a}|Trigger_selector {value::a,maybe_value::Maybe a}|Any_selector {strict::Bool,maybe_value::Maybe a,selector::Selector a}|Default_selector {strict::Bool,maybe_value::Maybe a,selector::Selector a}|Group_any_selector {maybe_value::Maybe a,selector::Selector a}|Group_selector {maybe_value::Maybe a,group_selector::DIM.IntMap (Selector a)}|Widget_trigger_selector {maybe_value::Maybe a,selector::Selector a}|Widget_io_trigger_selector {maybe_value::Maybe a,selector::Selector a}|Widget_mix_trigger_selector {maybe_value::Maybe a,selector::Selector a}|Coroutine_any_selector {maybe_value::Maybe a,selector::Selector a}|Coroutine_selector {maybe_value::Maybe a,coroutine_selector::DIM.IntMap (Selector a)}

data Insert a=Insert {insert_strategy::Insert_strategy,value::a}

data Border a=Border {left::a,down::a,right::a,up::a}

data Data=Data_bool {bool::Bool}|Data_int {int::Int}

data Submit=Submit {maybe_album_id::Maybe Int,vertex::DSeq.Seq Vertex,index::DSeq.Seq DW.Word32,parameter::Parameter,vertex_length::DW.Word32,index_length::DW.Word32}

data Program_counter=Program_counter {code_index::Int,clone_index::Int}

data Album=Album {width::DW.Word32,height::DW.Word32,texture::FP.Ptr SDLT.SDL_GPUTexture}

data Timer=Off|On {timer_id::DW.Word32,interval::DW.Word64}

data Window=Window {window_id::Int,sdl_window_id::DW.Word32,sdl_window::FP.Ptr SDLT.SDL_Window,graphics_pipeline::FP.Ptr SDLT.SDL_GPUGraphicsPipeline,design_width::FCT.CFloat,design_height::FCT.CFloat,adaptive_width::FCT.CFloat,adaptive_height::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Row=Blank|Row {row_core::DSeq.Seq Character,x::FCT.CFloat,y::FCT.CFloat,width::FCT.CFloat,min_down::FCT.CFloat,max_up::FCT.CFloat,min_descent::FCT.CFloat,max_ascent::FCT.CFloat}

data Character=Character {unicode::Int,font_id::Int,size::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Sentence=Sentence {sentence_core::DSeq.Seq Phrase,path::String}

data Phrase=Phrase {phrase_core::DT.Text,size::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Visual=Triangle {first_point::Point,second_point::Point,third_point::Point}|Convex_polygon {point::DSeq.Seq Point}|Regular_polygon {number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture {width::FCT.CFloat,height::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,path::String,locked::Bool}|Large_picture {width::FCT.CFloat,height::FCT.CFloat,album_id::Int}|Atlas {clip_request::DSeq.Seq Clip_request,path::String,clip::DVS.Vector Clip,index::Int,locked::Bool}|Large_atlas {clip::DVS.Vector Clip,album_id::Int,index::Int}|Animation {delay::DVS.Vector FCT.CFloat,moment::FCT.CFloat,frame_width::FCT.CFloat,frame_height::FCT.CFloat,width::FCT.CFloat,height::FCT.CFloat,padding::FCT.CFloat,width_number::Int,height_number::Int,album_number::Int,album_id::Int,count::Int,index::Int}

data Visual_request=Triangle_request {first_point::Point,second_point::Point,third_point::Point}|Convex_polygon_request {point::DSeq.Seq Point}|Regular_polygon_request {number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture_request {path::String}|Large_picture_request {path::String}|Atlas_request {clip_request::DSeq.Seq Clip_request,path::String}|Large_atlas_request {clip_request::DSeq.Seq Clip_request,path::String}|Animation_request {min_delay::FCT.CFloat,width::DW.Word32,height::DW.Word32,padding::Int,path::String}

data Clip_request=Clip_request {x::FCT.CFloat,y::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Matrix=Matrix {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat}

data Point=Point {x::FCT.CFloat,y::FCT.CFloat}

data Atlas=Leaf_atlas {border::Border DW.Word32,used::Bool}|Node_atlas {border::Border DW.Word32,left_atlas::Atlas,right_atlas::Atlas}

data Glyph=Glyph {advance::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Font=Font {descent::FCT.CFloat,ascent::FCT.CFloat,glyph::DIM.IntMap Glyph}

data Projection_strategy=Object_strategy|Image_strategy|Image_safe_strategy

data Projection_path=Object_path {leaf_id::Int}|Image_path {leaf_id::Int}|Image_safe_path {leaf_id::Int}

data Projection_move=Object_move {leaf_id::Int,consume::Bool}|Image_move {leaf_id::Int}|Image_safe_move {leaf_id::Int}

data Insert_strategy=Min_strategy|Max_strategy|Index_strategy {seat::Int}

data Action=Close|Resize {width::FCT.CFloat,height::FCT.CFloat}|Press {press::Press,change::Key,maintain::DSet.Set Key}

data Press=Press_up|Press_down

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z deriving (Eq,Ord)

data Window_flag=Window_fullscreen|Window_hidden|Window_borderless|Window_resizable|Window_always_on_top deriving (Eq,Ord)

data Extended=Negative_infinity|Finite {number::FCT.CFloat}|Positive_infinity deriving (Eq,Ord)

data Clip=Clip {x::FCT.CFloat,y::FCT.CFloat,width::FCT.CFloat,height::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

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
    width<-FS.peekByteOff ptr 8
    height<-FS.peekByteOff ptr 12
    min_u<-FS.peekByteOff ptr 16
    min_v<-FS.peekByteOff ptr 20
    max_u<-FS.peekByteOff ptr 24
    max_v<-FS.peekByteOff ptr 28
    return (Clip {x=x,y=y,width=width,height=height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v})

clip_poke::FP.Ptr Clip->Clip->IO ()
clip_poke ptr clip=case clip of
    Clip {x,y,width,height,min_u,min_v,max_u,max_v}->let new_ptr=FP.castPtr ptr in do
        FS.pokeElemOff new_ptr 0 x
        FS.pokeElemOff new_ptr 1 y
        FS.pokeElemOff new_ptr 2 width
        FS.pokeElemOff new_ptr 3 height
        FS.pokeElemOff new_ptr 4 min_u
        FS.pokeElemOff new_ptr 5 min_v
        FS.pokeElemOff new_ptr 6 max_u
        FS.pokeElemOff new_ptr 7 max_v

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
    Layout {address,size}->let new_ptr=FP.castPtr ptr in do
        FS.pokeElemOff new_ptr 0 address
        FS.pokeElemOff new_ptr 1 size

data Vertex=Vertex {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,parameter_id::FCT.CFloat,size::FCT.CFloat}

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
    Vertex {red,green,blue,alpha,x,y,u,v,parameter_id,size}->let new_ptr=FP.castPtr ptr in do
        FS.pokeElemOff new_ptr 0 red
        FS.pokeElemOff new_ptr 1 green
        FS.pokeElemOff new_ptr 2 blue
        FS.pokeElemOff new_ptr 3 alpha
        FS.pokeElemOff new_ptr 4 x
        FS.pokeElemOff new_ptr 5 y
        FS.pokeElemOff new_ptr 6 u
        FS.pokeElemOff new_ptr 7 v
        FS.pokeElemOff new_ptr 8 parameter_id
        FS.pokeElemOff new_ptr 9 size

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
    Parameter {x,y,x_x,x_y,y_x,y_y,border_flag,border_left,border_down,border_right,border_up}->let new_ptr=FP.castPtr ptr::FP.Ptr FCT.CFloat in do
        FS.pokeElemOff new_ptr 0 x
        FS.pokeElemOff new_ptr 1 y
        FS.pokeElemOff new_ptr 2 x_x
        FS.pokeElemOff new_ptr 3 x_y
        FS.pokeElemOff new_ptr 4 y_x
        FS.pokeElemOff new_ptr 5 y_y
        FS.pokeElemOff new_ptr 6 0
        FS.pokeElemOff new_ptr 7 border_flag
        FS.pokeElemOff new_ptr 8 border_left
        FS.pokeElemOff new_ptr 9 border_down
        FS.pokeElemOff new_ptr 10 border_right
        FS.pokeElemOff new_ptr 11 border_up

class Custom_request a where
    custom_request::a->Engine b c a d e->IO (Engine b c a d e)

class Custom_widget a where
    custom_widget_run::Event b->Engine c b d a e->a->(a,Engine c b d a e->Engine c b d a e,Event b->Engine c b d a e->Maybe Int)
    custom_widget_collect::FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->a->Submit
    custom_widget_remove::a->Engine b c d a e->IO (Engine b c d a e)
    custom_widget_lock::a->a
    custom_widget_unlock::a->Engine b c d a e->IO (Engine b c d a e,a)

class Custom_widget_request a where
    custom_widget_request::a->Engine b c d e a->IO (Engine b c d e a,e)