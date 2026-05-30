{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Engine.Type where

import qualified SDL.Type as T
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Set as DSet
import qualified Data.Sequence as DSeq
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP

data Engine a=Engine {state::a,active::DIM.IntMap (Active a),free::DIM.IntMap (Free a),bound::DIM.IntMap (Bound a),node::DIM.IntMap (Node a),window::DIM.IntMap Window,window_map::DM.Map DW.Word32 Int,request::DSeq.Seq (Request a),key::DSet.Set Key,main_id::Engine a->Event->Maybe Int,timer::Timer}

data Active a=Active {next::Engine a->Event->Maybe Int,ancestry::DSeq.Seq Int,widget::Widget a}

data Free a=Free {ancestry::DSeq.Seq Int,widget::Widget a}

data Bound a=Bound {window_id::Int,ancestry::DSeq.Seq Int,widget::Widget a}

data Node a=Node {active_child::DIS.IntSet,free_child::DIS.IntSet,bound_child::DIS.IntSet,node_child::DIS.IntSet,ancestry::DSeq.Seq Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Request a->Widget a->Widget a}

data Widget a=Trigger {trigger::Event->Engine a->Engine a}|Io_trigger {io_trigger::Event->Engine a->IO (Engine a)}

data Request a=Create_widget {father::Maybe Int,widget_request::Widget_request a,widget_id::Int}|Remove_widget {widget_type::Widget_type,widget_id::Int}|Create_node {father::Maybe Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Request a->Widget a->Widget a,node_id::Int}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::DT.Text,width::FCT.CInt,height::FCT.CInt,window_flag::DSet.Set Window_flag}|Remove_window {window_id::Int}|Io {io::Engine a->IO (Engine a)}

data Widget_request a=Trigger_request {next::Engine a->Event->Maybe Int,trigger::Event->Engine a->Engine a}|Io_trigger_request {next::Engine a->Event->Maybe Int,io_trigger::Event->Engine a->IO (Engine a)}

data Timer=Keep_off|Keep_on {time::DW.Word64}|Turn_off|Turn_on {time::DW.Word64}

data Widget_type=Active_widget|Free_widget|Bound_widget

data Window=Window {window_id::Int,sdl_window_id::DW.Word32,sdl_window::FP.Ptr T.SDL_window,sdl_renderer::FP.Ptr T.SDL_renderer,window_bound::DIS.IntSet}

data Window_flag=Window_fullscreen|Window_hidden|Window_borderless|Window_resizable

data Event=Unknown|Quit|Time|At {window_id::Int,action::Action}

data Action=Close|Press {press::Press,keycode::Key,set_keycode::DSet.Set Key}

data Press=Press_up|Press_down

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z deriving (Eq,Ord)