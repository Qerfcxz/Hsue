{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Engine.Type where

import qualified SDL.Type as T
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Set as DSet
import qualified Data.Sequence as DSeq
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

data Engine a=Engine {state::a,active::DIM.IntMap (Active a),free::DIM.IntMap (Free a),bound::DIM.IntMap (Bound a),node::DIM.IntMap (Node a),window::DIM.IntMap Window,window_map::DM.Map DW.Word32 Int,request::DSeq.Seq (Request a),key::DSet.Set Key,main_id::Engine a->Event->Maybe Int,timer::Timer,device::FP.Ptr T.SDL_GPUDevice,vertex_shader::FP.Ptr T.SDL_GPUShader,fragment_shader::FP.Ptr T.SDL_GPUShader}

data Active a=Active {next::Engine a->Event->Maybe Int,ancestry::DSeq.Seq Int,widget::Widget a}

data Free a=Free {ancestry::DSeq.Seq Int,widget::Widget a}

data Bound a=Bound {window_id::Int,ancestry::DSeq.Seq Int,widget::Widget a}

data Node a=Node {active_child::DIS.IntSet,free_child::DIS.IntSet,bound_child::DIS.IntSet,node_child::DIS.IntSet,ancestry::DSeq.Seq Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Request a->Widget a->Widget a}

data Widget a=Trigger {trigger::Event->Engine a->Engine a}|Io_trigger {io_trigger::Event->Engine a->IO (Engine a)}|Collector {graph::DIM.IntMap Graph}|Geometry {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,geometry::Geometry}

data Request a=Create_widget {father::Maybe Int,widget_request::Widget_request a,widget_id::Int}|Remove_widget {widget_type::Widget_type,widget_id::Int}|Create_node {father::Maybe Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Request a->Widget a->Widget a,node_id::Int}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::String,width::FCT.CInt,height::FCT.CInt,window_flag::DSet.Set Window_flag}|Remove_window {window_id::Int}|Clear_window {window_id::Int,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}|Io {io::Engine a->IO (Engine a)}|Render {bound_id::Int}

data Widget_request a=Trigger_request {next::Engine a->Event->Maybe Int,trigger::Event->Engine a->Engine a}|Io_trigger_request {next::Engine a->Event->Maybe Int,io_trigger::Event->Engine a->IO (Engine a)}|Collector_request|Geometry_request {window_id::Int,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,geometry::Geometry}

data Timer=Keep_off|Keep_on {time::DW.Word64}|Turn_off|Turn_on {time::DW.Word64}

data Widget_type=Active_widget|Free_widget|Bound_widget

data Window=Window {window_id::Int,sdl_window_id::DW.Word32,sdl_window::FP.Ptr T.SDL_Window,triangle_graphics_pipeline::FP.Ptr T.SDL_GPUGraphicsPipeline,window_bound::DIS.IntSet}

data Geometry=Triangle {first_x::FCT.CFloat,first_y::FCT.CFloat,second_x::FCT.CFloat,second_y::FCT.CFloat,third_x::FCT.CFloat,third_y::FCT.CFloat}

data Window_flag=Window_fullscreen|Window_hidden|Window_borderless|Window_resizable

data Event=Unknown|Quit|Time|At {window_id::Int,action::Action}

data Action=Close|Press {press::Press,keycode::Key,set_keycode::DSet.Set Key}

data Press=Press_up|Press_down

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z deriving (Eq,Ord)

data Graph=Pack (DSeq.Seq Graph)|Single_Triangle {first_vertex::Vertex,second_vertex::Vertex,third_vertex::Vertex}|Multiple_Triangle {vertex::DSeq.Seq Vertex,index::DSeq.Seq DW.Word32}

data Vertex=Vertex {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat}

instance FS.Storable Vertex where
    sizeOf _=24
    alignment _=4
    peek _=error "peek: error 1"
    poke ptr (Vertex {red,green,blue,alpha,x,y})=do
        let new_ptr=FP.castPtr ptr::FP.Ptr FCT.CFloat
        FS.pokeElemOff new_ptr 0 red
        FS.pokeElemOff new_ptr 1 green
        FS.pokeElemOff new_ptr 2 blue
        FS.pokeElemOff new_ptr 3 alpha
        FS.pokeElemOff new_ptr 4 x
        FS.pokeElemOff new_ptr 5 y