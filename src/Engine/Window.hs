{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Window where

import Engine.Container
import Engine.Helper
import Engine.Type
import qualified SDL.Function as F
import qualified Data.IntSet as DIS
import qualified Foreign.C.Types as FCT

remove_window::Int->Engine a->IO (Engine a)
remove_window window_id engine=let (new_window,window)=intmap_delete_lookup window_id engine.window in case window of
    Window {sdl_window_id,sdl_window,graphics_pipeline}->do
        catch_false (F.sdl_wait_for_gpu_idle engine.device)
        F.sdl_release_window_from_gpu_device engine.device sdl_window
        F.sdl_release_gpu_graphics_pipeline engine.device graphics_pipeline
        F.sdl_destroy_window sdl_window
        return (engine {window=new_window,window_map=map_delete sdl_window_id engine.window_map})

adaptive_window::FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(FCT.CFloat,FCT.CFloat)
adaptive_window design_width design_height width height=if design_width*height<design_height*width then (design_height/height*width,design_height) else (design_width,design_width/width*height)

create_adaptive_window_trigger_request::(Engine a->Event->Maybe Int)->DIS.IntSet->Widget_request a
create_adaptive_window_trigger_request next this_window_id=Trigger_request {next=next,trigger=create_adaptive_window_trigger_request_a this_window_id}

create_adaptive_window_trigger_request_a::DIS.IntSet->Event->Engine a->Engine a
create_adaptive_window_trigger_request_a this_window_id event engine=case event of
    At {window_id,action}->case action of
        Resize {width,height}->if DIS.member window_id this_window_id then engine {window=intmap_update window_id (create_adaptive_window_trigger_request_b width height) engine.window} else engine
        _->engine
    _->engine

create_adaptive_window_trigger_request_b::FCT.CFloat->FCT.CFloat->Window->Window
create_adaptive_window_trigger_request_b width height window=case window of
    Window {design_width,design_height}->let (adaptive_width,adaptive_height)=adaptive_window design_width design_height width height in window {adaptive_width=adaptive_width,adaptive_height=adaptive_height}