{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Window where

import Engine.Container
import Engine.Type
import Engine.Underlying
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified Data.IntSet as DIS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT

remove_window::Int->Engine a b c d e->IO (Engine a b c d e)
remove_window window_id engine=let (window,single_window)=intmap_delete_lookup window_id engine.window in case single_window of
    Window {sdl_window_id,sdl_window,graphics_pipeline}->do
        catch_false (SDLF.sdl_wait_for_gpu_idle engine.device)
        SDLF.sdl_release_window_from_gpu_device engine.device sdl_window
        SDLF.sdl_release_gpu_graphics_pipeline engine.device graphics_pipeline
        SDLF.sdl_destroy_window sdl_window
        return (engine {window=window,window_map=map_delete sdl_window_id engine.window_map})

adaptive_window::FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(FCT.CFloat,FCT.CFloat)
adaptive_window design_width design_height width height=if design_width*height<design_height*width then (design_height/height*width,design_height) else (design_width,design_width/width*height)

create_adaptive_window_trigger_request::(Event a->Engine b a c d e->Maybe Int)->DIS.IntSet->Widget_request b a c d e
create_adaptive_window_trigger_request next window_id=Trigger_request {next=next,trigger=create_adaptive_window_trigger_request_a window_id}

create_adaptive_window_trigger_request_a::DIS.IntSet->Event a->Engine b a c d e->Engine b a c d e
create_adaptive_window_trigger_request_a this_window_id event engine=case event of
    At {window_id,action}->case action of
        Resize {width,height}->if DIS.member window_id this_window_id then engine {window=intmap_update window_id (create_adaptive_window_trigger_request_b width height) engine.window} else engine
        _->engine
    _->engine

create_adaptive_window_trigger_request_b::FCT.CFloat->FCT.CFloat->Window->Window
create_adaptive_window_trigger_request_b width height window=case window of
    Window {design_width,design_height}->let (adaptive_width,adaptive_height)=adaptive_window design_width design_height width height in window {adaptive_width=adaptive_width,adaptive_height=adaptive_height,width=width,height=height}

from_window_flag::Window_flag->DW.Word64
from_window_flag window_flag=case window_flag of
    Window_fullscreen->SDLI.sdl_window_fullscreen
    Window_hidden->SDLI.sdl_window_hidden
    Window_borderless->SDLI.sdl_window_borderless
    Window_resizable->SDLI.sdl_window_resizable
    Window_always_on_top->SDLI.sdl_window_always_on_top