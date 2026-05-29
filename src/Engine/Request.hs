{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Request where

import Engine.Type
import Engine.Window
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.Ptr as FP

create_request::Request a->Engine a->Engine a
create_request request engine=engine {request=engine.request DS.|> request}

do_request::Request a->Engine a->IO (Engine a)
do_request request engine=case request of
    Create_window {window_id,title,width,height,window_flag}->DBS.useAsCString (DTE.encodeUtf8 title) $ \cstring->do
        sdl_window<-F.sdl_createwindow cstring width height (DF.foldl' (\word flag->word DB..|. from_window_flag flag) 0 window_flag)
        if sdl_window==FP.nullPtr then error "do_request: error 1" else do
            sdl_renderer<-F.sdl_createrenderer sdl_window FP.nullPtr
            if sdl_renderer==FP.nullPtr then error "do_request: error 2" else do
                sdl_window_id<-F.sdl_getwindowid sdl_window
                let (maybe_window,new_window)=DIM.insertLookupWithKey (\_ window _->window) window_id (Window {window_id=window_id,sdl_window_id=sdl_window_id,sdl_window=sdl_window,sdl_renderer=sdl_renderer,window_bound=DIS.empty}) engine.window in case maybe_window of
                    Nothing->return (engine {window=new_window,window_map=DM.insert sdl_window_id window_id engine.window_map})
                    _->error "do_request: error 3"
    Remove_window {window_id}->remove_window window_id engine
    Io {io}->io engine

from_window_flag::Window_flag->DW.Word64
from_window_flag window_flag=case window_flag of
    Window_fullscreen->C.sdl_window_fullscreen
    Window_hidden->C.sdl_window_hidden
    Window_borderless->C.sdl_window_borderless
    Window_resizable->C.sdl_window_resizable