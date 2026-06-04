{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Engine where

import Engine.Backup
import Engine.Other
import Engine.Request
import Engine.Shader
import Engine.Type
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified SDL.Type as T
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Word as DW
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

init_engine::IO ()
init_engine=catch_error (F.sdl_init C.sdl_init_video)

quit_engine::IO ()
quit_engine=F.sdl_quit

create_engine::Backup_strategy->Int->Maybe DW.Word32->(Engine a->Event->Maybe Int)->a->IO (Engine a)
create_engine backup count timer main_id state=do
    device<-F.sdl_creategpudevice C.sdl_gpu_shaderformat_dxil (FMU.fromBool True) FP.nullPtr
    if device==FP.nullPtr then error "create_engine: error 1" else do
        vertex_shader<-load_shader device C.sdl_gpu_shaderformat_dxil C.sdl_gpu_shaderstage_vertex "Vertex.cso"
        fragment_shader<-load_shader device C.sdl_gpu_shaderformat_dxil C.sdl_gpu_shaderstage_fragment "Fragment.cso"
        event_number<-F.sdl_registerevents 1
        callback<-F.wrapper $ \_ _ time->do
            FMA.allocaBytesAligned C.sdl_event_size C.sdl_event_alignment $ \ptr->do
                FMU.fillBytes ptr 0 C.sdl_event_size
                FS.poke (FP.castPtr ptr) event_number
                catch_error (F.sdl_pushevent ptr)
            return time
        case timer of
            Nothing->return (Engine {state=state,active=DIM.empty,free=DIM.empty,bound=DIM.empty,node=DIM.empty,window=DIM.empty,window_map=DM.empty,request=DSeq.empty,key=DSet.empty,main_id=main_id,backup_strategy=backup,event_number=event_number,callback=callback,device=device,vertex_shader=vertex_shader,fragment_shader=fragment_shader,count=count,timer=Nothing})
            Just time->if 0<time
                then do
                    new_timer<-F.sdl_addtimer time callback FP.nullPtr
                    return (Engine {state=state,active=DIM.empty,free=DIM.empty,bound=DIM.empty,node=DIM.empty,window=DIM.empty,window_map=DM.empty,request=DSeq.empty,key=DSet.empty,main_id=main_id,backup_strategy=backup,event_number=event_number,callback=callback,device=device,vertex_shader=vertex_shader,fragment_shader=fragment_shader,count=count,timer=Just new_timer})
                else error "create_engine: error 2"

clean_engine::Engine a->IO ()
clean_engine engine=do
    DF.mapM_ (clean_window engine.device) (DIM.elems engine.window)
    case engine.timer of
        Nothing->return ()
        Just timer->catch_error (F.sdl_removetimer timer)
    FP.freeHaskellFunPtr engine.callback
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
run_engine engine=FMA.allocaBytesAligned C.sdl_event_size C.sdl_event_alignment $ \event->case engine.timer of
    Nothing->loop_engine event engine
    Just _->loop_engine_time event engine

loop_engine::FP.Ptr ()->Engine a->IO ()
loop_engine event engine=do
    new_engine<-run_request engine
    value<-F.sdl_waitevent event
    if FMU.toBool value
        then do
            event_type<-C.sdl_event_type event
            loop_event event_type event new_engine
        else error "loop_engine: error 1"

loop_engine_time::FP.Ptr ()->Engine a->IO ()
loop_engine_time event engine=do
    new_engine<-run_request engine
    value<-F.sdl_waitevent event
    if FMU.toBool value
        then do
            event_type<-C.sdl_event_type event
            if event_type==engine.event_number then loop_event_a (Time {tick=new_engine.count}) event (new_engine {count=new_engine.count+1}) else loop_event event_type event new_engine
        else error "loop_engine_time: error 1"

loop_event::DW.Word32->FP.Ptr ()->Engine a->IO ()
loop_event event_type event engine=case event_type of
    C.SDL_EVENT_QUIT->return ()
    C.SDL_EVENT_WINDOW_CLOSE_REQUESTED->do
        sdl_window_id<-C.sdl_windowevent_windowid event
        case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a Unknown event engine
            Just window_id->loop_event_a (At {window_id=window_id,action=Close}) event engine
    C.SDL_EVENT_KEY_UP->do
        sdl_window_id<-C.sdl_keyboardevent_windowid event
        sdl_keycode<-C.sdl_keyboardevent_key event
        let key=to_key sdl_keycode in let new_key=DSet.delete key engine.key in let new_engine=engine {key=new_key} in case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a Unknown event engine
            Just window_id->loop_event_a (At {window_id=window_id,action=Press {press=Press_up,change=key,maintain=new_key}}) event new_engine
    C.SDL_EVENT_KEY_DOWN->do
        sdl_window_id<-C.sdl_keyboardevent_windowid event
        sdl_keycode<-C.sdl_keyboardevent_key event
        let key=to_key sdl_keycode in let new_key=DSet.insert key engine.key in let new_engine=engine {key=new_key} in case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a Unknown event engine
            Just window_id->loop_event_a (At {window_id=window_id,action=Press {press=Press_down,change=key,maintain=new_key}}) event new_engine
    _->loop_event_a Unknown event engine

loop_event_a::Event->FP.Ptr ()->Engine a->IO ()
loop_event_a event ptr engine=let new_engine=run_event event engine in case new_engine.timer of
    Nothing->loop_engine ptr new_engine
    Just _->loop_engine_time ptr new_engine

to_key::DW.Word32->Key
to_key key=case key of
    C.SDLK_A->Key_a
    C.SDLK_B->Key_b
    C.SDLK_C->Key_c
    C.SDLK_D->Key_d
    C.SDLK_E->Key_e
    C.SDLK_F->Key_f
    C.SDLK_G->Key_g
    C.SDLK_H->Key_h
    C.SDLK_I->Key_i
    C.SDLK_J->Key_j
    C.SDLK_K->Key_k
    C.SDLK_L->Key_l
    C.SDLK_M->Key_m
    C.SDLK_N->Key_n
    C.SDLK_O->Key_o
    C.SDLK_P->Key_p
    C.SDLK_Q->Key_q
    C.SDLK_R->Key_r
    C.SDLK_S->Key_s
    C.SDLK_T->Key_t
    C.SDLK_U->Key_u
    C.SDLK_V->Key_v
    C.SDLK_W->Key_w
    C.SDLK_X->Key_x
    C.SDLK_Y->Key_y
    C.SDLK_Z->Key_z
    _->Key_unknown

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