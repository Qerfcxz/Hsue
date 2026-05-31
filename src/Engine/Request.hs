{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Request where

import Engine.Node
import Engine.Other
import Engine.Type
import Engine.Widget
import Engine.Window
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.Ptr as FP

create_request::Request a->Engine a->Engine a
create_request request engine=engine {request=engine.request DS.|> request}

do_request::Request a->Engine a->IO (Engine a)
do_request request engine=case request of
    Create_widget {father,widget_request,widget_id}->case widget_request of
        Trigger_request {}->create_active father widget_request widget_id engine
        Io_trigger_request {}->create_active father widget_request widget_id engine
        Geometry_request {}->create_bound father widget_request widget_id engine
    Remove_widget {widget_type,widget_id}->case widget_type of
        Active_widget->remove_active widget_id engine
        Free_widget->remove_free widget_id engine
        Bound_widget->remove_bound widget_id engine
    Create_node {father,event_transform,widget_transform,node_id}->return (create_node father event_transform widget_transform node_id engine)
    Remove_node {node_id}->remove_node node_id engine
    Create_window {window_id,title,width,height,window_flag}->DBS.useAsCString (DTE.encodeUtf8 title) $ \cstring->do
        sdl_window<-F.sdl_createwindow cstring width height (DF.foldl' (\word flag->word DB..|. from_window_flag flag) 0 window_flag)
        if sdl_window==FP.nullPtr then error "do_request: error 3" else do
            catch_error (F.sdl_claimwindowforgpudevice engine.device sdl_window)
            sdl_window_id<-F.sdl_getwindowid sdl_window
            let (maybe_window,new_window)=DIM.insertLookupWithKey (\_ window _->window) window_id (Window {window_id=window_id,sdl_window_id=sdl_window_id,sdl_window=sdl_window,window_bound=DIS.empty}) engine.window in case maybe_window of
                Nothing->return (engine {window=new_window,window_map=map_insert sdl_window_id window_id engine.window_map})
                _->error "do_request: error 4"
    Remove_window {window_id}->remove_window window_id engine
    Io {io}->io engine
    Render {bound_id}->case DIM.lookup bound_id engine.bound of
        Nothing->error "do_request: error 5"
        Just (Bound {window_id,ancestry,widget})->case do_widget_transform ancestry engine request widget of
            Geometry {red,green,blue,alpha,geometry}->return engine
            _->error "do_request: error 6"

do_widget_transform::DS.Seq Int->Engine a->Request a->Widget a->Widget a
do_widget_transform ancestry engine request widget=DF.foldl' (\this_widget node_id->do_widget_transform_a node_id engine.node engine request this_widget) widget ancestry

do_widget_transform_a::Int->DIM.IntMap (Node a)->Engine a->Request a->Widget a->Widget a
do_widget_transform_a node_id engine_node engine request widget=case DIM.lookup node_id engine_node of
    Nothing->error "do_widget_transform_a: error 1"
    Just node->node.widget_transform engine request widget

from_window_flag::Window_flag->DW.Word64
from_window_flag window_flag=case window_flag of
    Window_fullscreen->C.sdl_window_fullscreen
    Window_hidden->C.sdl_window_hidden
    Window_borderless->C.sdl_window_borderless
    Window_resizable->C.sdl_window_resizable