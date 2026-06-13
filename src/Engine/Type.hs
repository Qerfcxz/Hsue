{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Engine.Type where

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
import qualified Foreign.Storable as FS

data Engine a=Engine {state::a,main_id::Engine a->Event->Maybe Int,projection_strategy::Engine a->Event->Projection_strategy,callback::FP.FunPtr (FP.Ptr ()->DW.Word32->DW.Word64->IO DW.Word64),atlas::Atlas,album::DIM.IntMap Album,active::DIM.IntMap (Active a),inactive::DIM.IntMap (Inactive a),node::DIM.IntMap (Node a),window::DIM.IntMap Window,window_map::DM.Map DW.Word32 Int,request::DSeq.Seq (Request a),key::DSet.Set Key,device::FP.Ptr T.SDL_GPUDevice,texture::FP.Ptr T.SDL_GPUTexture,sampler::FP.Ptr T.SDL_GPUSampler,vertex_shader::FP.Ptr T.SDL_GPUShader,fragment_shader::FP.Ptr T.SDL_GPUShader,vertex_buffer::FP.Ptr T.SDL_GPUBuffer,index_buffer::FP.Ptr T.SDL_GPUBuffer,transfer_buffer::FP.Ptr T.SDL_GPUTransferBuffer,picture_transfer_buffer::FP.Ptr T.SDL_GPUTransferBuffer,picture_size::FCT.CInt,vertex_size::Int,index_size::Int,initial_album_id::Int,album_id::Int,count::Int,timer::Timer,time::DW.Word64,event_number::DW.Word32,padding::DW.Word32,width::DW.Word32,height::DW.Word32,reciprocal_width::FCT.CFloat,reciprocal_height::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat}

data Active a=Active {ancestry::DSeq.Seq Int,projection::Projection (Widget a),next::Engine a->Event->Maybe Int}

data Inactive a=Inactive {ancestry::DSeq.Seq Int,projection::Projection (Widget a)}

data Node a=Node {ancestry::DSeq.Seq Int,active_child::DIS.IntSet,inactive_child::DIS.IntSet,node_child::DIS.IntSet,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}

data Widget a=Trigger {trigger::Event->Engine a->Engine a}|Io_trigger {io_trigger::Event->Engine a->IO (Engine a)}|Collector {initial_min_index::Int,initial_max_index::Int,min_index::Int,max_index::Int,submit::DIM.IntMap (DSeq.Seq Submit)}|Visual {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,matrix::Matrix,visual::Visual}

data Request a=Reset_timer {interval::DW.Word64}|Stop_timer|Stop_timer_safe|Create_widget {widget_id::Int,father::Maybe Int,widget_request::Widget_request a}|Remove_widget {widget_id::Int,widget_type::Bool}|Create_node {node_id::Int,father::Maybe Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::DT.Text,width::FCT.CInt,height::FCT.CInt,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,window_flag::DSet.Set Window_flag}|Remove_window {window_id::Int}|Clean_atlas|Reload_atlas {inactive_id::Int}|Render {window_id::Int,projection_move::Projection_move}|Io {io::Engine a->IO (Engine a)}

data Widget_request a=Trigger_request {trigger::Event->Engine a->Engine a,next::Engine a->Event->Maybe Int}|Io_trigger_request {io_trigger::Event->Engine a->IO (Engine a),next::Engine a->Event->Maybe Int}|Collector_request {initial_min_index::Int,initial_max_index::Int}|Visual_request {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,matrix::Matrix,visual_request::Visual_request}

data Projection a=Without {object::a}|With {object::a,image::a}

data Album=Album {width::DW.Word32,height::DW.Word32,texture::FP.Ptr T.SDL_GPUTexture}

data Timer=Off|On {timer_id::DW.Word32,interval::DW.Word64}

data Window=Window {window_id::Int,sdl_window_id::DW.Word32,sdl_window::FP.Ptr T.SDL_Window,triangle_graphics_pipeline::FP.Ptr T.SDL_GPUGraphicsPipeline,design_width::FCT.CFloat,design_height::FCT.CFloat,adaptive_width::FCT.CFloat,adaptive_height::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Visual=Triangle {first_point::Point,second_point::Point,third_point::Point}|Convex_polygon {point::DSeq.Seq Point}|Regular_polygon {number::Int,center::Point,radius::FCT.CFloat,angle::FCT.CFloat}|Picture {left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,album_id::Int,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}|Large_picture {left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,album_id::Int}|Locked_picture {left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,album_id::Int}

data Visual_request=Triangle_request {first_point::Point,second_point::Point,third_point::Point}|Convex_polygon_request {point::DSeq.Seq Point}|Regular_polygon_request {number::Int,center::Point,radius::FCT.CFloat,angle::FCT.CFloat}|Picture_request {center::Point,path::String}|Large_picture_request {center::Point,path::String}

data Matrix=Matrix {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat}

data Point=Point {x::FCT.CFloat,y::FCT.CFloat}

data Rectangle=Rectangle {left::DW.Word32,down::DW.Word32,right::DW.Word32,up::DW.Word32}

data Atlas=Leaf_atlas {rectangle::Rectangle,used::Bool}|Node_atlas {rectangle::Rectangle,left_atlas::Atlas,right_atlas::Atlas}

data Window_flag=Window_fullscreen|Window_hidden|Window_borderless|Window_resizable

data Projection_strategy=Object_strategy|Image_strategy|Image_safe_strategy

data Projection_path=Object_path {projection_id::Int}|Image_path {projection_id::Int}|Image_safe_path {projection_id::Int}

data Projection_move=Object_move {consume::Bool,projection_id::Int}|Image_move {projection_id::Int}|Image_safe_move {projection_id::Int}

data Collect_strategy=Min_collect_strategy|Max_collect_strategy|Index_collect_strategy {seat::Int}

data Event=Quit|Time {tick::Int,time::DW.Word64,interval::DW.Word64}|At {window_id::Int,action::Action}

data Action=Close|Resize {width::FCT.CFloat,height::FCT.CFloat}|Press {press::Press,change::Key,maintain::DSet.Set Key}

data Press=Press_up|Press_down

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z deriving (Eq,Ord)

data Submit=Submit {maybe_album_id::Maybe Int,vertex::DSeq.Seq Vertex,index::DSeq.Seq DW.Word32,vertex_length::DW.Word32,index_length::DW.Word32}

data Vertex=Vertex {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat}

instance FS.Storable Vertex where
    sizeOf _=32
    alignment _=4
    peek _=error "peek: error 1"
    poke ptr vertex=case vertex of
        (Vertex {red,green,blue,alpha,x,y,u,v})->let new_ptr=FP.castPtr ptr::FP.Ptr FCT.CFloat in do
            FS.pokeElemOff new_ptr 0 red
            FS.pokeElemOff new_ptr 1 green
            FS.pokeElemOff new_ptr 2 blue
            FS.pokeElemOff new_ptr 3 alpha
            FS.pokeElemOff new_ptr 4 x
            FS.pokeElemOff new_ptr 5 y
            FS.pokeElemOff new_ptr 6 u
            FS.pokeElemOff new_ptr 7 v