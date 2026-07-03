{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Engine.Type where

import qualified SDL.Type as T
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DAT
import qualified Data.Bits as DB
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

newtype Dynamic_bool a=Dynamic_bool {dynamic_bool::Engine a->Widget a->Bool}

instance Eq (Dynamic_bool a) where
    (==)=dynamic_bool_equal

dynamic_bool_equal::Dynamic_bool a->Dynamic_bool a->Bool
dynamic_bool_equal _ _=error "dynamic_bool_equal: error 1"

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

dynamic_bool_and::Dynamic_bool a->Dynamic_bool a->Dynamic_bool a
dynamic_bool_and first_dynamic_bool second_dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_binary_operator (DB..&.) first_dynamic_bool.dynamic_bool second_dynamic_bool.dynamic_bool}

dynamic_bool_or::Dynamic_bool a->Dynamic_bool a->Dynamic_bool a
dynamic_bool_or first_dynamic_bool second_dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_binary_operator (DB..|.) first_dynamic_bool.dynamic_bool second_dynamic_bool.dynamic_bool}

dynamic_bool_xor::Dynamic_bool a->Dynamic_bool a->Dynamic_bool a
dynamic_bool_xor first_dynamic_bool second_dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_binary_operator DB.xor first_dynamic_bool.dynamic_bool second_dynamic_bool.dynamic_bool}

dynamic_bool_complement::Dynamic_bool a->Dynamic_bool a
dynamic_bool_complement dynamic_bool=Dynamic_bool {dynamic_bool=dynamic_bool_unary_operator DB.complement dynamic_bool.dynamic_bool}

dynamic_bool_shift::Dynamic_bool a->Int->Dynamic_bool a
dynamic_bool_shift dynamic_bool int=if int==0 then dynamic_bool else Dynamic_bool {dynamic_bool=dynamic_bool_false}

dynamic_bool_rotate::Dynamic_bool a->Int->Dynamic_bool a
dynamic_bool_rotate dynamic_bool _=dynamic_bool

dynamic_bool_bit_size::Dynamic_bool a->Int
dynamic_bool_bit_size _=1

dynamic_bool_bit_size_maybe::Dynamic_bool a->Maybe Int
dynamic_bool_bit_size_maybe _=Just 1

dynamic_bool_is_signed::Dynamic_bool a->Bool
dynamic_bool_is_signed _=False

dynamic_bool_test_bit::Dynamic_bool a->Int->Bool
dynamic_bool_test_bit _ _=error "dynamic_bool_test_bit: error 1"

dynamic_bool_bit::Int->Dynamic_bool a
dynamic_bool_bit int=if int==0 then Dynamic_bool {dynamic_bool=dynamic_bool_true} else Dynamic_bool {dynamic_bool=dynamic_bool_false}

dynamic_bool_pop_count::Dynamic_bool a->Int
dynamic_bool_pop_count _=error "dynamic_bool_pop_count: error 1"

dynamic_bool_true::Engine a->Widget a->Bool
dynamic_bool_true _ _=True

dynamic_bool_false::Engine a->Widget a->Bool
dynamic_bool_false _ _=False

dynamic_bool_unary_operator::(Bool->Bool)->(Engine a->Widget a->Bool)->Engine a->Widget a->Bool
dynamic_bool_unary_operator operator dynamic_bool engine widget=operator (dynamic_bool engine widget)

dynamic_bool_binary_operator::(Bool->Bool->Bool)->(Engine a->Widget a->Bool)->(Engine a->Widget a->Bool)->Engine a->Widget a->Bool
dynamic_bool_binary_operator operator first_dynamic_bool second_dynamic_bool engine widget=operator (first_dynamic_bool engine widget) (second_dynamic_bool engine widget)

newtype Dynamic_int a=Dynamic_int {dynamic_int::Engine a->Widget a->Int}

instance Num (Dynamic_int a) where
    (+)=dynamic_int_addition
    (*)=dynamic_int_multiplication
    abs=dynamic_int_abs
    signum=dynamic_int_signum
    fromInteger=dynamic_int_from_integer
    negate=dynamic_int_negate

dynamic_int_addition::Dynamic_int a->Dynamic_int a->Dynamic_int a
dynamic_int_addition first_dynamic_int second_dynamic_int=Dynamic_int {dynamic_int=dynamic_int_binary_operator (+) first_dynamic_int.dynamic_int second_dynamic_int.dynamic_int}

dynamic_int_multiplication::Dynamic_int a->Dynamic_int a->Dynamic_int a
dynamic_int_multiplication first_dynamic_int second_dynamic_int=Dynamic_int {dynamic_int=dynamic_int_binary_operator (*) first_dynamic_int.dynamic_int second_dynamic_int.dynamic_int}

dynamic_int_abs::Dynamic_int a->Dynamic_int a
dynamic_int_abs dynamic_int=Dynamic_int {dynamic_int=dynamic_int_unary_operator abs dynamic_int.dynamic_int}

dynamic_int_signum::Dynamic_int a->Dynamic_int a
dynamic_int_signum dynamic_int=Dynamic_int {dynamic_int=dynamic_int_unary_operator signum dynamic_int.dynamic_int}

dynamic_int_from_integer::Integer->Dynamic_int a
dynamic_int_from_integer integer=Dynamic_int {dynamic_int=const (const (fromInteger integer))}

dynamic_int_negate::Dynamic_int a->Dynamic_int a
dynamic_int_negate dynamic_int=Dynamic_int {dynamic_int=dynamic_int_unary_operator negate dynamic_int.dynamic_int}

dynamic_int_unary_operator::(Int->Int)->(Engine a->Widget a->Int)->Engine a->Widget a->Int
dynamic_int_unary_operator operator dynamic_int engine widget=operator (dynamic_int engine widget)

dynamic_int_binary_operator::(Int->Int->Int)->(Engine a->Widget a->Int)->(Engine a->Widget a->Int)->Engine a->Widget a->Int
dynamic_int_binary_operator operator first_dynamic_int second_dynamic_int engine widget=operator (first_dynamic_int engine widget) (second_dynamic_int engine widget)

data Extended a=Negative_infinity|Finite {number::a}|Positive_infinity deriving (Eq,Ord)

data Engine a=Engine {state::a,main_id::Engine a->Event->Maybe Int,projection_strategy::Engine a->Event->Projection_strategy,callback::FP.FunPtr (FP.Ptr ()->DW.Word32->DW.Word64->IO DW.Word64),atlas::Atlas,album::DIM.IntMap Album,leaf::DIM.IntMap (Projection a),node::DIM.IntMap (Node a),window::DIM.IntMap Window,font::DIM.IntMap Font,window_map::DM.Map DW.Word32 Int,font_map::DM.Map String Int,request::DSeq.Seq (Request a),key::DSet.Set Key,device::FP.Ptr T.SDL_GPUDevice,texture::FP.Ptr T.SDL_GPUTexture,sampler::FP.Ptr T.SDL_GPUSampler,vertex_shader::FP.Ptr T.SDL_GPUShader,fragment_shader::FP.Ptr T.SDL_GPUShader,vertex_buffer::FP.Ptr T.SDL_GPUBuffer,index_buffer::FP.Ptr T.SDL_GPUBuffer,parameter_buffer::FP.Ptr T.SDL_GPUBuffer,transfer_buffer::FP.Ptr T.SDL_GPUTransferBuffer,picture_transfer_buffer::FP.Ptr T.SDL_GPUTransferBuffer,picture_size::FCT.CInt,vertex_size::Int,index_size::Int,parameter_size::Int,initial_album_id::Int,album_id::Int,initial_font_id::Int,font_id::Int,count::Int,timer::Timer,time::DW.Word64,event_number::DW.Word32,padding::DW.Word32,width::DW.Word32,height::DW.Word32,reciprocal_width::FCT.CFloat,reciprocal_height::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,font_size::FCT.CFloat,pixel_range::FCT.CFloat}

data Projection a=Without {ancestry_id::DSeq.Seq Int,object::Widget a}|With {ancestry_id::DSeq.Seq Int,object::Widget a,image::Widget a}

data Node a=Node {ancestry_id::DSeq.Seq Int,leaf_child::DIS.IntSet,node_child::DIS.IntSet,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}

data Widget a=Double {which::Bool,first_widget::Widget a,second_widget::Widget a}|Group {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,index::Int,group_widget::DIM.IntMap (Widget a)}|Trigger {next::Engine a->Event->Maybe Int,trigger::Event->Engine a->Engine a}|Io_trigger {next::Engine a->Event->Maybe Int,io_trigger::Event->Engine a->IO (Engine a)}|Mix_trigger {next::Engine a->Event->Maybe Int,mix_trigger::Event->(Engine a->Engine a,Engine a->IO (Engine a)),order::Bool}|Widget_trigger {next::Engine a->Event->Maybe Int,widget_trigger::Widget a->Event->Engine a->(Engine a->Engine a,Widget a),widget::Widget a}|Widget_io_trigger {next::Engine a->Event->Maybe Int,widget_io_trigger::Widget a->Event->Engine a->(Engine a->IO (Engine a),Widget a),widget::Widget a}|Widget_mix_trigger {next::Engine a->Event->Maybe Int,widget_mix_trigger::Widget a->Event->Engine a->(Engine a->Engine a,Engine a->IO (Engine a),Widget a),order::Bool,widget::Widget a}|Coroutine {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,index::Int,coroutine_state::DIM.IntMap (Coroutine_state a),linear_coroutine::DIM.IntMap (Linear_coroutine a)}|Store {store::Data}|Collector {initial_min_index::Int,min_index::Int,initial_max_index::Int,max_index::Int,submit::DIM.IntMap (DSeq.Seq Submit)}|Visual {origin::Point,matrix::Matrix,maybe_clip::Maybe Clip,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,visual::Visual}|Text {origin::Point,matrix::Matrix,width::FCT.CFloat,height::FCT.CFloat,y::FCT.CFloat,max_y::FCT.CFloat,article::DSeq.Seq (DSeq.Seq Row),charset::DM.Map String (DSet.Set Char),locked::Bool}

data Request a=Reset_timer {interval::DW.Word64}|Stop_timer|Stop_timer_safe|Create_widget {leaf_id::Int,maybe_father_id::Maybe Int,widget_request::Widget_request a}|Remove_widget {leaf_id::Int}|Create_node {node_id::Int,maybe_father_id::Maybe Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::DT.Text,width::FCT.CInt,height::FCT.CInt,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,window_flag::DSet.Set Window_flag}|Remove_window {window_id::Int}|Clean_atlas|Unlock {leaf_id::Int}|Load_charset {charset::DM.Map String (DSet.Set Char)}|Render {window_id::Int,projection_move::Projection_move}|Io {io::Engine a->IO (Engine a)}

data Widget_request a=Double_request {which::Bool,first_widget_request::Widget_request a,second_widget_request::Widget_request a}|Group_request {initial_min_index::Int,initial_max_index::Int,index::Int,insert_widget_request::DSeq.Seq (Insert a (Widget_request a))}|Trigger_request {next::Engine a->Event->Maybe Int,trigger::Event->Engine a->Engine a}|Io_trigger_request {next::Engine a->Event->Maybe Int,io_trigger::Event->Engine a->IO (Engine a)}|Mix_trigger_request {next::Engine a->Event->Maybe Int,mix_trigger::Event->(Engine a->Engine a,Engine a->IO (Engine a)),order::Bool}|Widget_trigger_request {next::Engine a->Event->Maybe Int,widget_trigger::Widget a->Event->Engine a->(Engine a->Engine a,Widget a),widget_request::Widget_request a}|Widget_io_trigger_request {next::Engine a->Event->Maybe Int,widget_io_trigger::Widget a->Event->Engine a->(Engine a->IO (Engine a),Widget a),widget_request::Widget_request a}|Widget_mix_trigger_request {next::Engine a->Event->Maybe Int,widget_mix_trigger::Widget a->Event->Engine a->(Engine a->Engine a,Engine a->IO (Engine a),Widget a),order::Bool,widget_request::Widget_request a}|Coroutine_request {initial_min_index::Int,initial_max_index::Int,index::Int,insert_widget_request::DSeq.Seq (Insert a (Widget_request a)),raw_coroutine::Raw_coroutine a ()}|Store_request {store::Data}|Collector_request {initial_min_index::Int,initial_max_index::Int}|Visual_request {origin::Point,matrix::Matrix,maybe_clip::Maybe Clip,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,visual_request::Visual_request}|Text_request {origin::Point,matrix::Matrix,width::FCT.CFloat,height::FCT.CFloat,article::DSeq.Seq (DSeq.Seq Sentence),calculate_width::Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat,calculate_typesetting::Int->DSeq.Seq (DSeq.Seq Row)->(FCT.CFloat,FCT.CFloat),load::Bool}

data Coroutine_state a=Coroutine_state {widget::Widget a,variable::DIM.IntMap Int,program_counter::DIM.IntMap Program_counter,index_group::DIM.IntMap (DSeq.Seq Int),main_index_group::DSeq.Seq Int,index_group_index::Int,program_counter_index::Int}

data Coroutine a=Done|Emit {emit::Engine a->Widget a->(Widget a,Engine a)}|Wait {dynamic_int::Dynamic_int a}|Forever {coroutine::Coroutine a}|Then {coroutine_sequence::DSeq.Seq (Coroutine a)}|While {dynamic_bool::Dynamic_bool a,coroutine::Coroutine a}|Pause {dynamic_bool::Dynamic_bool a,coroutine::Coroutine a}|Skip {dynamic_bool::Dynamic_bool a,coroutine::Coroutine a}|Repeat {dynamic_int::Dynamic_int a,coroutine::Coroutine a}|Clone {int::Int,coroutine::Coroutine a}|If {dynamic_bool::Dynamic_bool a,first_coroutine::Coroutine a,second_coroutine::Coroutine a}|Dynamic_clone {dynamic_int::Dynamic_int a,int::Int,coroutine::Coroutine a}|Case {dynamic_int::Dynamic_int a,int::Int,coroutine_sequence::DSeq.Seq (Coroutine a)}|Fork {int::Int,coroutine::Coroutine a,coroutine_sequence::DSeq.Seq (Coroutine a)}|Race {dynamic_int::Dynamic_int a,first_int::Int,second_int::Int,coroutine_sequence::DSeq.Seq (Coroutine a)}

data Linear_coroutine a=Linear_end|Linear_yield|Linear_emit {emit::Engine a->Widget a->(Widget a,Engine a)}|Linear_wait {int_index::Int}|Linear_countdown {int_index::Int}|Linear_fork_kill {int_index::Int}|Linear_wake {int_index::Int}|Linear_fork {code_index::Int}|Linear_jump {code_index::Int}|Linear_one_less_jump {int_index::Int,code_index::Int}|Linear_one_more_jump {int_index::Int,code_index::Int}|Linear_clone_kill {int_index::Int,clone_number::Int}|Linear_dynamic_int {int_index::Int,dynamic_int::Dynamic_int a}|Linear_int {int_index::Int,int::Int}|Linear_kill_group {int_index::Int,int::Int}|Linear_true_jump {code_index::Int,dynamic_bool::Dynamic_bool a}|Linear_false_jump {code_index::Int,dynamic_bool::Dynamic_bool a}|Linear_less_jump {int_index::Int,code_index::Int,int::Int}|Linear_create_group {int_index::Int,group_code_index::DIM.IntMap Int,int::Int}|Linear_clone {int_index::Int,clone_number::Int,int::Int}|Linear_wake_group {int_index::Int,dynamic_int::Dynamic_int a,int::Int}|Linear_dynamic_clone {int_index::Int,code_index::Int,clone_number::Int,dynamic_int::Dynamic_int a,int::Int}

data Raw_coroutine a b=Raw_coroutine {coroutine_sequence::DSeq.Seq (Coroutine a),value::b}

instance Functor (Raw_coroutine a) where
    fmap=raw_coroutine_fmap

raw_coroutine_fmap::(a->b)->Raw_coroutine c a->Raw_coroutine c b
raw_coroutine_fmap function raw_coroutine=case raw_coroutine of
    Raw_coroutine {coroutine_sequence,value}->Raw_coroutine {coroutine_sequence=coroutine_sequence,value=function value}

instance Applicative (Raw_coroutine a) where
    pure=raw_coroutine_pure
    (<*>)=raw_coroutine_apply

raw_coroutine_pure::a->Raw_coroutine b a
raw_coroutine_pure value=Raw_coroutine {coroutine_sequence=DSeq.empty,value=value}

raw_coroutine_apply::Raw_coroutine a (b->c)->Raw_coroutine a b->Raw_coroutine a c
raw_coroutine_apply first_raw_coroutine second_raw_coroutine=case first_raw_coroutine of
    Raw_coroutine {coroutine_sequence=first_coroutine_sequence,value=function}->case second_raw_coroutine of
        Raw_coroutine {coroutine_sequence=second_coroutine_sequence,value=value}->Raw_coroutine {coroutine_sequence=first_coroutine_sequence DSeq.>< second_coroutine_sequence,value=function value}

instance Monad (Raw_coroutine a) where
    return=pure
    (>>=)=raw_coroutine_bind

raw_coroutine_bind::Raw_coroutine a b->(b->Raw_coroutine a c)->Raw_coroutine a c
raw_coroutine_bind raw_coroutine function=case raw_coroutine of
    Raw_coroutine {coroutine_sequence,value}->case function value of
        Raw_coroutine {coroutine_sequence=new_coroutine_sequence,value=new_value}->Raw_coroutine {coroutine_sequence=coroutine_sequence DSeq.>< new_coroutine_sequence,value=new_value}

data Insert a b=Insert {insert_strategy::Insert_strategy,value::b}

data Data=Data_bool {bool::Bool}|Data_int {int::Int}

data Submit=Submit {maybe_album_id::Maybe Int,vertex::DSeq.Seq Vertex,index::DSeq.Seq DW.Word32,parameter::Parameter,vertex_length::DW.Word32,index_length::DW.Word32}

data Program_counter=Program_counter {code_index::Int,clone_index::Int}

data Album=Album {width::DW.Word32,height::DW.Word32,texture::FP.Ptr T.SDL_GPUTexture}

data Timer=Off|On {timer_id::DW.Word32,interval::DW.Word64}

data Window=Window {window_id::Int,sdl_window_id::DW.Word32,sdl_window::FP.Ptr T.SDL_Window,graphics_pipeline::FP.Ptr T.SDL_GPUGraphicsPipeline,design_width::FCT.CFloat,design_height::FCT.CFloat,adaptive_width::FCT.CFloat,adaptive_height::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Window_flag=Window_fullscreen|Window_hidden|Window_borderless|Window_resizable

data Row=Blank|Row {row_core::DSeq.Seq Character,x::FCT.CFloat,y::FCT.CFloat,width::FCT.CFloat,min_down::FCT.CFloat,max_up::FCT.CFloat,min_descent::FCT.CFloat,max_ascent::FCT.CFloat}

data Character=Character {unicode::Int,font_id::Int,size::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Sentence=Sentence {sentence_core::DSeq.Seq Phrase,path::String}

data Phrase=Phrase {phrase_core::DT.Text,size::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Visual=Triangle {first_point::Point,second_point::Point,third_point::Point}|Convex_polygon {point::DSeq.Seq Point}|Regular_polygon {number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture {width::FCT.CFloat,height::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,path::String,locked::Bool}|Large_picture {width::FCT.CFloat,height::FCT.CFloat,album_id::Int}

data Visual_request=Triangle_request {first_point::Point,second_point::Point,third_point::Point}|Convex_polygon_request {point::DSeq.Seq Point}|Regular_polygon_request {number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture_request {path::String}|Large_picture_request {path::String}

data Clip=Clip {left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat}

data Matrix=Matrix {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat}

data Point=Point {x::FCT.CFloat,y::FCT.CFloat}

data Rectangle=Rectangle {left::DW.Word32,down::DW.Word32,right::DW.Word32,up::DW.Word32}

data Atlas=Leaf_atlas {rectangle::Rectangle,used::Bool}|Node_atlas {rectangle::Rectangle,left_atlas::Atlas,right_atlas::Atlas}

data Glyph=Glyph {advance::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Font=Font {descent::FCT.CFloat,ascent::FCT.CFloat,glyph::DIM.IntMap Glyph}

data Projection_strategy=Object_strategy|Image_strategy|Image_safe_strategy

data Projection_path=Object_path {leaf_id::Int}|Image_path {leaf_id::Int}|Image_safe_path {leaf_id::Int}

data Projection_move=Object_move {leaf_id::Int,consume::Bool}|Image_move {leaf_id::Int}|Image_safe_move {leaf_id::Int}

data Insert_strategy=Min_strategy|Max_strategy|Index_strategy {seat::Int}

data Event=Quit|Time {tick::Int,time::DW.Word64,interval::DW.Word64}|At {window_id::Int,action::Action}

data Action=Close|Resize {width::FCT.CFloat,height::FCT.CFloat}|Press {press::Press,change::Key,maintain::DSet.Set Key}

data Press=Press_up|Press_down

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z deriving (Eq,Ord)

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
vertex_peek _=error "vertex_peek: error 1"

vertex_poke::FP.Ptr Vertex->Vertex->IO ()
vertex_poke ptr vertex=case vertex of
    (Vertex {red,green,blue,alpha,x,y,u,v,parameter_id,size})->let new_ptr=FP.castPtr ptr in do
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

data Parameter=Parameter {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat,clip_flag::FCT.CFloat,clip_left::FCT.CFloat,clip_down::FCT.CFloat,clip_right::FCT.CFloat,clip_up::FCT.CFloat}

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
parameter_peek _=error "parameter_peek: error 1"

parameter_poke::FP.Ptr Parameter->Parameter->IO ()
parameter_poke ptr parameter=case parameter of
    (Parameter {x,y,x_x,x_y,y_x,y_y,clip_flag,clip_left,clip_down,clip_right,clip_up})->let new_ptr=FP.castPtr ptr::FP.Ptr FCT.CFloat in do
        FS.pokeElemOff new_ptr 0 x
        FS.pokeElemOff new_ptr 1 y
        FS.pokeElemOff new_ptr 2 x_x
        FS.pokeElemOff new_ptr 3 x_y
        FS.pokeElemOff new_ptr 4 y_x
        FS.pokeElemOff new_ptr 5 y_y
        FS.pokeElemOff new_ptr 6 0
        FS.pokeElemOff new_ptr 7 clip_flag
        FS.pokeElemOff new_ptr 8 clip_left
        FS.pokeElemOff new_ptr 9 clip_down
        FS.pokeElemOff new_ptr 10 clip_right
        FS.pokeElemOff new_ptr 11 clip_up

data MSDF_Metrics=MSDF_Metrics {msdf_ascender::FCT.CFloat,msdf_descender::FCT.CFloat}

instance DA.FromJSON MSDF_Metrics where
    parseJSON=msdf_metrics_parse_json

msdf_metrics_parse_json::DAT.Value->DAT.Parser MSDF_Metrics
msdf_metrics_parse_json=DA.withObject "MSDF_Metrics" $ \object->do
    ascender<-object DA..: "ascender"::DAT.Parser Double
    descender<-object DA..: "descender"::DAT.Parser Double
    return (MSDF_Metrics {msdf_ascender=realToFrac ascender,msdf_descender=realToFrac descender})

data MSDF_Bounds=MSDF_Bounds {msdf_left::FCT.CFloat,msdf_bottom::FCT.CFloat,msdf_right::FCT.CFloat,msdf_top::FCT.CFloat}

instance DA.FromJSON MSDF_Bounds where
    parseJSON=msdf_bounds_parse_json

msdf_bounds_parse_json::DAT.Value->DAT.Parser MSDF_Bounds
msdf_bounds_parse_json=DA.withObject "MSDF_Bounds" $ \object->do
    left<-object DA..: "left"::DAT.Parser Double
    bottom<-object DA..: "bottom"::DAT.Parser Double
    right<-object DA..: "right"::DAT.Parser Double
    top<-object DA..: "top"::DAT.Parser Double
    return (MSDF_Bounds {msdf_left=realToFrac left,msdf_bottom=realToFrac bottom,msdf_right=realToFrac right,msdf_top=realToFrac top})

data MSDF_Glyph=MSDF_Glyph {msdf_unicode::Int,msdf_advance::FCT.CFloat,msdf_plane_bounds::MSDF_Bounds,msdf_atlas_bounds::MSDF_Bounds}

instance DA.FromJSON MSDF_Glyph where
    parseJSON=msdf_glyph_parse_json

msdf_glyph_parse_json::DAT.Value->DAT.Parser MSDF_Glyph
msdf_glyph_parse_json=DA.withObject "MSDF_Glyph" $ \object->do
    unicode<-object DA..: "unicode"
    advance<-object DA..: "advance"::DAT.Parser Double
    plane_bound<-(object DA..:? "planeBounds") DA..!= MSDF_Bounds {msdf_left=0,msdf_bottom=0,msdf_right=0,msdf_top=0}
    atlas_bound<-(object DA..:? "atlasBounds") DA..!= MSDF_Bounds {msdf_left=0,msdf_bottom=0,msdf_right=0,msdf_top=0}
    return (MSDF_Glyph {msdf_unicode=unicode,msdf_advance=realToFrac advance,msdf_plane_bounds=plane_bound,msdf_atlas_bounds=atlas_bound})

data MSDF_Output=MSDF_Output {msdf_metrics::MSDF_Metrics,msdf_glyphs::DSeq.Seq MSDF_Glyph}

instance DA.FromJSON MSDF_Output where
    parseJSON=msdf_output_parse_json

msdf_output_parse_json::DAT.Value->DAT.Parser MSDF_Output
msdf_output_parse_json=DA.withObject "MSDF_Output" $ \object->do
    metric<-object DA..: "metrics"
    glyph<-object DA..: "glyphs"
    return (MSDF_Output {msdf_metrics=metric,msdf_glyphs=glyph})