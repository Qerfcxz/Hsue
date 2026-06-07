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

data Engine a=Engine {state::a,active::DIM.IntMap (Active a),free::DIM.IntMap (Free a),bound::DIM.IntMap (Bound a),node::DIM.IntMap (Node a),window::DIM.IntMap Window,window_map::DM.Map DW.Word32 Int,request::DSeq.Seq (Request a),key::DSet.Set Key,main_id::Engine a->Event->Maybe Int,backup_strategy::Backup_strategy,count::Int,time::DW.Word64,timer::Timer,event_number::DW.Word32,callback::FP.FunPtr (FP.Ptr ()->DW.Word32->DW.Word64->IO DW.Word64),device::FP.Ptr T.SDL_GPUDevice,vertex_shader::FP.Ptr T.SDL_GPUShader,fragment_shader::FP.Ptr T.SDL_GPUShader,vertex_buffer::FP.Ptr T.SDL_GPUBuffer,index_buffer::FP.Ptr T.SDL_GPUBuffer,transfer_buffer::FP.Ptr T.SDL_GPUTransferBuffer,vertex_size::Int,index_size::Int}

data Active a=Active {next::Engine a->Event->Maybe Int,ancestry::DSeq.Seq Int,backup::Backup (Widget a)}

data Free a=Free {ancestry::DSeq.Seq Int,backup::Backup (Widget a)}

data Bound a=Bound {window_id::Int,ancestry::DSeq.Seq Int,backup::Backup (Widget a)}

data Node a=Node {active_child::DIS.IntSet,free_child::DIS.IntSet,bound_child::DIS.IntSet,node_child::DIS.IntSet,ancestry::DSeq.Seq Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}

data Widget a=Trigger {trigger::Event->Engine a->Engine a}|Io_trigger {io_trigger::Event->Engine a->IO (Engine a)}|Collector {initial_min_index::Int,initial_max_index::Int,min_index::Int,max_index::Int,graph::DIM.IntMap (DSeq.Seq Graph)}|Geometry {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,matrix::Matrix,geometry::Geometry}

data Request a=Reset_timer {interval::DW.Word64}|Stop_timer|Stop_timer_safe|Create_widget {father::Maybe Int,widget_request::Widget_request a,widget_id::Int}|Remove_widget {widget_type::Widget_type,widget_id::Int}|Create_node {father::Maybe Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a,node_id::Int}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::DT.Text,width::FCT.CInt,height::FCT.CInt,window_flag::DSet.Set Window_flag}|Remove_window {window_id::Int}|Render {backup_path::Backup_path,window_id::Int,submit_strategy::Submit_strategy}|Io {io::Engine a->IO (Engine a)}

data Widget_request a=Trigger_request {next::Engine a->Event->Maybe Int,trigger::Event->Engine a->Engine a}|Io_trigger_request {next::Engine a->Event->Maybe Int,io_trigger::Event->Engine a->IO (Engine a)}|Collector_request {initial_min_index::Int,initial_max_index::Int}|Geometry_request {window_id::Int,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,matrix::Matrix,geometry::Geometry}

data Backup a=Single {one::a}|Double {one::a,two::a}

data Timer=Off|On {timer_id::DW.Word32,interval::DW.Word64}

data Widget_type=Active_widget|Free_widget|Bound_widget

data Window=Window {window_id::Int,sdl_window_id::DW.Word32,sdl_window::FP.Ptr T.SDL_Window,triangle_graphics_pipeline::FP.Ptr T.SDL_GPUGraphicsPipeline,window_bound::DIS.IntSet}

data Geometry=Triangle {first_point::Point,second_point::Point,third_point::Point}|Convex_polygon {point::DSeq.Seq Point}|Regular_polygon {number::Int,center::Point,radius::FCT.CFloat,angle::FCT.CFloat}

data Matrix=Matrix {x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat}

data Point=Point {x::FCT.CFloat,y::FCT.CFloat}

data Window_flag=Window_fullscreen|Window_hidden|Window_borderless|Window_resizable

data Backup_strategy=One|Two|Two_safe

data Backup_path=One_path {backup_id::Int}|Two_path {backup_id::Int}|Two_safe_path {backup_id::Int}

data Collect_strategy=Min_collect|Max_collect|Index_collect {seat::Int}

data Move_strategy=Min_move {consume::Bool}|Max_move {consume::Bool}|Index_move {consume::Bool,seat::Int}

data Submit_strategy=Submit {consume::Bool}

data Event=Quit|Time {tick::Int,time::DW.Word64,interval::DW.Word64}|At {window_id::Int,action::Action}

data Action=Close|Press {press::Press,change::Key,maintain::DSet.Set Key}

data Press=Press_up|Press_down

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z deriving (Eq,Ord)

data Graph=Graph {vertex::DSeq.Seq Vertex,index::DSeq.Seq DW.Word32}

data Vertex=Vertex {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat}

instance FS.Storable Vertex where
    sizeOf _=24
    alignment _=4
    peek _=error "peek: error 1"
    poke pointer vertex=case vertex of
        (Vertex {red,green,blue,alpha,x,y})->let new_pointer=FP.castPtr pointer::FP.Ptr FCT.CFloat in do
            FS.pokeElemOff new_pointer 0 red
            FS.pokeElemOff new_pointer 1 green
            FS.pokeElemOff new_pointer 2 blue
            FS.pokeElemOff new_pointer 3 alpha
            FS.pokeElemOff new_pointer 4 x
            FS.pokeElemOff new_pointer 5 y