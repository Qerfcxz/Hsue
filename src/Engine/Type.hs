{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

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

data Engine a=Engine {state::a,leaf::DIM.IntMap (Leaf a),node::DIM.IntMap (Node a),window::DIM.IntMap Window,window_id_map::DM.Map DW.Word32 Int,request::DSeq.Seq (Request a),main_id::Engine a->Event->Maybe Int,timer::Timer}

data Leaf a=Leaf {father::Maybe Int,next::Engine a->Event->Maybe Int,widget::Widget a}

data Node a=Node {ancestry::DSeq.Seq Int,leaf_child::DIS.IntSet,node_child::DIS.IntSet,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}

data Widget a=Trigger {trigger::Event->Engine a->Engine a}|Io_trigger {io_trigger::Event->Engine a->IO (Engine a)}

data Request a=Create_window {window_id::Int,title::DT.Text,width::FCT.CInt,height::FCT.CInt,window_flag::DSet.Set Window_flag}|Io (Engine a->IO (Engine a))

data Timer=Keep_off|Keep_on {time::DW.Word64}|Turn_off|Turn_on {time::DW.Word64}

data Window=Window {window_id::Int,sdl_window::FP.Ptr T.SDL_window,sdl_renderer::FP.Ptr T.SDL_renderer,window_widget::DIS.IntSet}

data Event=Unknown|Quit|Time

data Window_flag=Window_resizable