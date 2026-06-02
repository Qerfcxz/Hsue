{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Engine where

import Engine.Backup
import Engine.Event
import Engine.Other
import Engine.Request
import Engine.Shader
import Engine.Type
import Engine.Window
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified SDL.Type as T
import qualified Data.Foldable as DF
import qualified Data.Int as DI
import qualified Data.IntMap as DIM
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Word as DW
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP

init_engine::IO ()
init_engine=catch_error (F.sdl_init C.sdl_init_video)

quit_engine::IO ()
quit_engine=F.sdl_quit

create_engine::Backup_strategy->Maybe DI.Int32->(Engine a->Event->Maybe Int)->a->IO (Engine a)
create_engine backup timer main_id state=do
    device<-F.sdl_creategpudevice C.sdl_gpu_shaderformat_dxil (FMU.fromBool True) FP.nullPtr
    if device==FP.nullPtr then error "create_engine: error 1" else do
        vertex_shader<-load_shader device C.sdl_gpu_shaderformat_dxil C.sdl_gpu_shaderstage_vertex "Vertex.cso"
        fragment_shader<-load_shader device C.sdl_gpu_shaderformat_dxil C.sdl_gpu_shaderstage_fragment "Fragment.cso"
        case timer of
            Nothing->return (Engine {state=state,active=DIM.empty,free=DIM.empty,bound=DIM.empty,node=DIM.empty,window=DIM.empty,window_map=DM.empty,request=DSeq.empty,key=DSet.empty,main_id=main_id,backup_strategy=backup,timer=Keep_off,device=device,vertex_shader=vertex_shader,fragment_shader=fragment_shader})
            Just time->return (Engine {state=state,active=DIM.empty,free=DIM.empty,bound=DIM.empty,node=DIM.empty,window=DIM.empty,window_map=DM.empty,request=DSeq.empty,key=DSet.empty,main_id=main_id,backup_strategy=backup,timer=Keep_on {time=fromIntegral time},device=device,vertex_shader=vertex_shader,fragment_shader=fragment_shader})

clean_engine::Engine a->IO ()
clean_engine engine=do
    DF.mapM_ (clean_window engine.device) (DIM.elems engine.window)
    F.sdl_releasegpushader engine.device engine.vertex_shader
    F.sdl_releasegpushader engine.device engine.fragment_shader
    F.sdl_destroygpudevice engine.device

clean_window::FP.Ptr T.SDL_GPUDevice->Window->IO ()
clean_window device window=do
    catch_error (F.sdl_waitforgpuidle device)
    F.sdl_releasewindowfromgpudevice device window.sdl_window
    F.sdl_releasegpugraphicspipeline device window.triangle_graphics_pipeline
    F.sdl_destroywindow window.sdl_window

run_engine::Engine a->IO ()
run_engine engine=FMA.allocaBytesAligned C.sdl_event_size C.sdl_event_alignment $ \ptr->case engine.timer of
    Keep_off->loop_engine ptr engine
    Keep_on {time}->do
        now<-F.sdl_getticks
        loop_engine_time time (now+time) ptr engine
    _->error "run_engine: error 1"

loop_engine::FP.Ptr ()->Engine a->IO ()
loop_engine ptr engine=do
    new_engine<-run_request engine
    (event,key)<-get_event ptr new_engine.window_map new_engine.key
    case event of
        Quit->return ()
        At window_id Close->do
            new_new_engine<-remove_window window_id (run_event event (new_engine {key=key}))
            loop_engine_a ptr new_new_engine
        _->loop_engine_a ptr (run_event event (new_engine {key=key}))

loop_engine_a::FP.Ptr ()->Engine a->IO ()
loop_engine_a ptr engine=case engine.timer of
    Keep_off->loop_engine ptr engine
    Turn_on {time}->do
        now<-F.sdl_getticks
        loop_engine_time time (now+time) ptr (engine {timer=Keep_on {time=time}})
    _->error "loop_engine: error 1"

loop_engine_time::DW.Word64->DW.Word64->FP.Ptr ()->Engine a->IO ()
loop_engine_time time next_time ptr engine=do
    new_engine<-run_request engine
    now<-F.sdl_getticks
    if now<next_time
        then do
            (event,key)<-get_event_time (fromIntegral (next_time-now)) ptr new_engine.window_map new_engine.key
            case event of
                Quit->return ()
                Time->loop_engine_time_a (next_time+time) ptr (run_event Time (new_engine {key=key}))
                At window_id Close->do
                    new_new_engine<-remove_window window_id (run_event event (new_engine {key=key}))
                    loop_engine_time_a next_time ptr new_new_engine
                _->loop_engine_time_a next_time ptr (run_event event (new_engine {key=key}))
        else loop_engine_time_a (max (next_time+time) now) ptr (run_event Time new_engine)

loop_engine_time_a::DW.Word64->FP.Ptr ()->Engine a->IO ()
loop_engine_time_a next_time ptr engine=case engine.timer of
    Keep_on {time}->loop_engine_time time next_time ptr engine
    Turn_off->loop_engine ptr (engine {timer=Keep_off})
    Turn_on {time}->do
        now<-F.sdl_getticks
        loop_engine_time time (now+time) ptr (engine {timer=Keep_on {time=time}})
    _->error "loop_engine_time_a: error 1"

run_request::Engine a->IO (Engine a)
run_request engine=case engine.request of
    DSeq.Empty->return engine
    (request DSeq.:<| other_request)->do
        new_engine<-do_request request (engine {request=other_request})
        run_request new_engine

run_event::Event->Engine a->Engine a
run_event event engine=case engine.main_id engine event of
    Nothing->engine
    Just main_id->run_event_a main_id event engine

run_event_a::Int->Event->Engine a->Engine a
run_event_a active_id event engine=let active=intmap_lookup active_id engine.active in let new_event=DF.foldl' (\this_event node_id->(intmap_lookup node_id engine.node).event_transform engine this_event) event active.ancestry in let new_engine=run_widget new_event (backup_lookup engine.backup_strategy active.backup) engine in case active.next new_engine new_event of
    Nothing->new_engine
    Just new_active_id->run_event_a new_active_id event new_engine

run_widget::Event->Widget a->Engine a->Engine a
run_widget event (Trigger {trigger}) engine=trigger event engine
run_widget event (Io_trigger {io_trigger}) engine=create_request (Io (io_trigger event)) engine
run_widget _ _ _=error "run_widget: error 1"