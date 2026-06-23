{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Engine.Type where

import Engine.Instance
import qualified SDL.Type as T
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP

data Engine a=Engine {state::a,main_id::Engine a->Event->Maybe Int,projection_strategy::Engine a->Event->Projection_strategy,callback::FP.FunPtr (FP.Ptr ()->DW.Word32->DW.Word64->IO DW.Word64),atlas::Atlas,album::DIM.IntMap Album,leaf::DIM.IntMap (Projection a),node::DIM.IntMap (Node a),window::DIM.IntMap Window,font::DIM.IntMap Font,window_map::DM.Map DW.Word32 Int,font_map::DM.Map String Int,request::DSeq.Seq (Request a),key::DSet.Set Key,device::FP.Ptr T.SDL_GPUDevice,texture::FP.Ptr T.SDL_GPUTexture,sampler::FP.Ptr T.SDL_GPUSampler,vertex_shader::FP.Ptr T.SDL_GPUShader,fragment_shader::FP.Ptr T.SDL_GPUShader,vertex_buffer::FP.Ptr T.SDL_GPUBuffer,index_buffer::FP.Ptr T.SDL_GPUBuffer,parameter_buffer::FP.Ptr T.SDL_GPUBuffer,transfer_buffer::FP.Ptr T.SDL_GPUTransferBuffer,picture_transfer_buffer::FP.Ptr T.SDL_GPUTransferBuffer,picture_size::FCT.CInt,vertex_size::Int,index_size::Int,parameter_size::Int,initial_album_id::Int,album_id::Int,initial_font_id::Int,font_id::Int,count::Int,timer::Timer,time::DW.Word64,event_number::DW.Word32,padding::DW.Word32,width::DW.Word32,height::DW.Word32,reciprocal_width::FCT.CFloat,reciprocal_height::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,font_size::FCT.CFloat,pixel_range::FCT.CFloat}

data Projection a=Without {ancestry_id::DSeq.Seq Int,object::Widget a}|With {ancestry_id::DSeq.Seq Int,object::Widget a,image::Widget a}

data Node a=Node {ancestry_id::DSeq.Seq Int,leaf_child::DIS.IntSet,node_child::DIS.IntSet,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}

data Widget a=Double {which::Bool,first_widget::Widget a,second_widget::Widget a}|Group {index::Int,group_widget::DIM.IntMap (Widget a)}|Trigger {next::Engine a->Event->Maybe Int,trigger::Event->Engine a->Engine a}|Io_trigger {next::Engine a->Event->Maybe Int,io_trigger::Event->Engine a->IO (Engine a)}|Mix_trigger {next::Engine a->Event->Maybe Int,mix_trigger::Event->(Engine a->Engine a,Engine a->IO (Engine a)),order::Bool}|Widget_trigger {next::Engine a->Event->Maybe Int,widget_trigger::Widget a->Event->Engine a->(Engine a->Engine a,Widget a),widget::Widget a}|Widget_io_trigger {next::Engine a->Event->Maybe Int,widget_io_trigger::Widget a->Event->Engine a->(Engine a->IO (Engine a),Widget a),widget::Widget a}|Widget_mix_trigger {next::Engine a->Event->Maybe Int,widget_mix_trigger::Widget a->Event->Engine a->(Engine a->Engine a,Engine a->IO (Engine a),Widget a),order::Bool,widget::Widget a}|Store {store::Data}|Collector {initial_min_index::Int,initial_max_index::Int,min_index::Int,max_index::Int,submit::DIM.IntMap (DSeq.Seq Submit)}|Visual {origin::Point,matrix::Matrix,maybe_clip::Maybe Clip,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,visual::Visual}|Text {origin::Point,matrix::Matrix,width::FCT.CFloat,height::FCT.CFloat,y::FCT.CFloat,max_y::FCT.CFloat,article::DSeq.Seq (DSeq.Seq Row),charset::DM.Map String (DSet.Set Char),locked::Bool}

data Request a=Reset_timer {interval::DW.Word64}|Stop_timer|Stop_timer_safe|Create_widget {leaf_id::Int,maybe_father_id::Maybe Int,widget_request::Widget_request a}|Remove_widget {leaf_id::Int}|Create_node {node_id::Int,maybe_father_id::Maybe Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::DT.Text,width::FCT.CInt,height::FCT.CInt,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,window_flag::DSet.Set Window_flag}|Remove_window {window_id::Int}|Clean_atlas|Unlock {leaf_id::Int}|Load_charset {charset::DM.Map String (DSet.Set Char)}|Render {window_id::Int,projection_move::Projection_move}|Io {io::Engine a->IO (Engine a)}

data Widget_request a=Double_request {which::Bool,first_widget_request::Widget_request a,second_widget_request::Widget_request a}|Group_request {index::Int,group_widget_request::DIM.IntMap (Widget_request a)}|Trigger_request {next::Engine a->Event->Maybe Int,trigger::Event->Engine a->Engine a}|Io_trigger_request {next::Engine a->Event->Maybe Int,io_trigger::Event->Engine a->IO (Engine a)}|Mix_trigger_request {next::Engine a->Event->Maybe Int,mix_trigger::Event->(Engine a->Engine a,Engine a->IO (Engine a)),order::Bool}|Widget_trigger_request {next::Engine a->Event->Maybe Int,widget_trigger::Widget a->Event->Engine a->(Engine a->Engine a,Widget a),widget_request::Widget_request a}|Widget_io_trigger_request {next::Engine a->Event->Maybe Int,widget_io_trigger::Widget a->Event->Engine a->(Engine a->IO (Engine a),Widget a),widget_request::Widget_request a}|Widget_mix_trigger_request {next::Engine a->Event->Maybe Int,widget_mix_trigger::Widget a->Event->Engine a->(Engine a->Engine a,Engine a->IO (Engine a),Widget a),order::Bool,widget_request::Widget_request a}|Store_request {store::Data}|Collector_request {initial_min_index::Int,initial_max_index::Int}|Visual_request {origin::Point,matrix::Matrix,maybe_clip::Maybe Clip,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,visual_request::Visual_request}|Text_request {origin::Point,matrix::Matrix,width::FCT.CFloat,height::FCT.CFloat,article::DSeq.Seq (DSeq.Seq Sentence),calculate_width::Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat,calculate_typesetting::Int->DSeq.Seq (DSeq.Seq Row)->(FCT.CFloat,FCT.CFloat),load::Bool}

data Extended a=Negative_infinity|Finite {number::a}|Positive_infinity deriving (Eq,Ord)

data Data=Data_bool {bool::Bool}|Data_int {int::Int}

data Submit=Submit {maybe_album_id::Maybe Int,vertex::DSeq.Seq Vertex,index::DSeq.Seq DW.Word32,parameter::Parameter,vertex_length::DW.Word32,index_length::DW.Word32}

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

data Collect_strategy=Min_strategy|Max_strategy|Index_strategy {seat::Int}

data Event=Quit|Time {tick::Int,time::DW.Word64,interval::DW.Word64}|At {window_id::Int,action::Action}

data Action=Close|Resize {width::FCT.CFloat,height::FCT.CFloat}|Press {press::Press,change::Key,maintain::DSet.Set Key}

data Press=Press_up|Press_down

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z deriving (Eq,Ord)