{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Engine where

import Engine.Atlas
import Engine.Other
import Engine.Projection
import Engine.Request
import Engine.Shader
import Engine.Type
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified SDL.Type as T
import qualified Data.Bits as DB
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

init_engine::IO ()
init_engine=do
    catch_false (FCS.withCString "SDL_TIMER_RESOLUTION" (FCS.withCString "1" . F.sdl_sethint))
    catch_false (F.sdl_init C.sdl_init_video)

quit_engine::IO ()
quit_engine=F.sdl_quit

create_engine::a->(Engine a->Event->Maybe Int)->(Engine a->Event->Projection_strategy)->FCT.CInt->Int->Int->Int->Int->Int->Maybe DW.Word64->DW.Word64->DW.Word32->DW.Word32->DW.Word32->IO (Engine a)
create_engine state main_id projection_strategy picture_size vertex_size index_size atlas_id album_id count maybe_interval time padding width height=if padding<0 then error "create_engine: error 1" else do
    device<-F.sdl_creategpudevice C.sdl_gpu_shaderformat_dxil (FMU.fromBool True) FP.nullPtr
    catch_null device
    vertex_shader<-load_shader device C.sdl_gpu_shaderformat_dxil C.sdl_gpu_shaderstage_vertex 0 1 "Vertex.cso"
    fragment_shader<-load_shader device C.sdl_gpu_shaderformat_dxil C.sdl_gpu_shaderstage_fragment 1 0 "Fragment.cso"
    texture<-FMU.with (C.SDL_GPUTextureCreateInfo {sdl_type=C.sdl_gpu_texturetype_2d,sdl_format=C.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=C.sdl_gpu_textureusage_sampler DB..|. C.sdl_gpu_textureusage_color_target,sdl_width=width,sdl_height=height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=C.sdl_gpu_samplecount_1}) (return_catch_null . F.sdl_creategputexture device)
    sampler<-FMU.with (C.SDL_GPUSamplerCreateInfo {sdl_min_filter=C.sdl_gpu_filter_nearest,sdl_mag_filter=C.sdl_gpu_filter_nearest,sdl_mipmap_mode=C.sdl_gpu_samplermipmapmode_linear,sdl_address_mode_u=C.sdl_gpu_sampleraddressmode_clamp_to_edge,sdl_address_mode_v=C.sdl_gpu_sampleraddressmode_clamp_to_edge,sdl_address_mode_w=C.sdl_gpu_sampleraddressmode_clamp_to_edge}) (return_catch_null . F.sdl_creategpusampler device)
    command_buffer<-F.sdl_acquiregpucommandbuffer device
    catch_null command_buffer
    FMU.with (C.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=C.SDL_FColor {sdl_r=0,sdl_g=0,sdl_b=0,sdl_a=0},sdl_load_op=C.sdl_gpu_loadop_clear,sdl_store_op=C.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-F.sdl_begingpurenderpass command_buffer color_target_info 1 FP.nullPtr
        catch_null render_pass
        F.sdl_endgpurenderpass render_pass
    catch_false (F.sdl_submitgpucommandbuffer command_buffer)
    let create_buffer=return_catch_null . F.sdl_creategpubuffer device
    let new_vertex_size=fromIntegral vertex_size
    let new_index_size=fromIntegral index_size
    vertex_buffer<-FMU.with (C.SDL_GPUBufferCreateInfo {sdl_usage=C.sdl_gpu_bufferusage_vertex,sdl_size=new_vertex_size}) create_buffer
    index_buffer<-FMU.with (C.SDL_GPUBufferCreateInfo {sdl_usage=C.sdl_gpu_bufferusage_index,sdl_size=new_index_size}) create_buffer
    transfer_buffer<-FMU.with (C.SDL_GPUTransferBufferCreateInfo {sdl_usage=C.sdl_gpu_transferbufferusage_upload,sdl_size=new_vertex_size+new_index_size}) (return_catch_null . F.sdl_creategputransferbuffer device)
    picture_transfer_buffer<-FMU.with (C.SDL_GPUTransferBufferCreateInfo {sdl_usage=C.sdl_gpu_transferbufferusage_upload,sdl_size=fromIntegral picture_size}) (return_catch_null . F.sdl_creategputransferbuffer device)
    event_number<-F.sdl_registerevents 1
    callback<-F.wrapper $ \_ _ interval->do
        FMA.allocaBytesAligned C.sdl_event_size C.sdl_event_alignment $ \ptr->do
            FMU.fillBytes ptr 0 C.sdl_event_size
            FS.poke (FP.castPtr ptr) event_number
            catch_false (F.sdl_pushevent ptr)
        return interval
    (new_texture,new_width,new_height)<-load_texture device picture_transfer_buffer picture_size "White.png"
    let (new_atlas,new_atlas_id,left,down,u,v)=atlas_insert_white new_width new_height padding (init_atlas width height atlas_id)
    copy_texture device new_texture texture left down new_width new_height
    let new_album_id=album_id+1 in case maybe_interval of
        Nothing->return (Engine {state=state,main_id=main_id,projection_strategy=projection_strategy,callback=callback,atlas=new_atlas,album=DIM.singleton album_id (Album {width=new_width,height=new_height,texture=new_texture}),active=DIM.empty,inactive=DIM.empty,node=DIM.empty,window=DIM.empty,window_map=DM.empty,request=DSeq.empty,key=DSet.empty,device=device,texture=texture,sampler=sampler,vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_buffer=vertex_buffer,index_buffer=index_buffer,transfer_buffer=transfer_buffer,picture_transfer_buffer=picture_transfer_buffer,picture_size=picture_size,vertex_size=vertex_size,index_size=index_size,initial_atlas_id=new_atlas_id,atlas_id=new_atlas_id,initial_album_id=new_album_id,album_id=new_album_id,count=count,timer=Off,time=time,event_number=event_number,padding=padding,u=u,v=v})
        Just interval->if 0<interval
            then do
                timer_id<-F.sdl_addtimerns interval callback FP.nullPtr
                catch_zero timer_id
                return (Engine {state=state,main_id=main_id,projection_strategy=projection_strategy,callback=callback,atlas=new_atlas,album=DIM.singleton album_id (Album {width=new_width,height=new_height,texture=new_texture}),active=DIM.empty,inactive=DIM.empty,node=DIM.empty,window=DIM.empty,window_map=DM.empty,request=DSeq.empty,key=DSet.empty,device=device,texture=texture,sampler=sampler,vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_buffer=vertex_buffer,index_buffer=index_buffer,transfer_buffer=transfer_buffer,picture_transfer_buffer=picture_transfer_buffer,picture_size=picture_size,vertex_size=vertex_size,index_size=index_size,initial_atlas_id=new_atlas_id,atlas_id=new_atlas_id,initial_album_id=new_album_id,album_id=new_album_id,count=count,timer=On {timer_id=timer_id,interval=interval},time=time,event_number=event_number,padding=padding,u=u,v=v})
            else error "create_engine: error 2"

clean_engine::Engine a->IO ()
clean_engine engine=do
    DF.mapM_ (clean_window engine.device) (DIM.elems engine.window)
    case engine.timer of
        Off->return ()
        On {timer_id}->catch_false (F.sdl_removetimer timer_id)
    FP.freeHaskellFunPtr engine.callback
    F.sdl_releasegputexture engine.device engine.texture
    F.sdl_releasegpusampler engine.device engine.sampler
    F.sdl_releasegpubuffer engine.device engine.vertex_buffer
    F.sdl_releasegpubuffer engine.device engine.index_buffer
    F.sdl_releasegputransferbuffer engine.device engine.transfer_buffer
    F.sdl_releasegputransferbuffer engine.device engine.picture_transfer_buffer
    F.sdl_releasegpushader engine.device engine.vertex_shader
    F.sdl_releasegpushader engine.device engine.fragment_shader
    F.sdl_destroygpudevice engine.device

clean_window::FP.Ptr T.SDL_GPUDevice->Window->IO ()
clean_window device window=do
    catch_false (F.sdl_waitforgpuidle device)
    F.sdl_releasewindowfromgpudevice device window.sdl_window
    F.sdl_releasegpugraphicspipeline device window.triangle_graphics_pipeline
    F.sdl_destroywindow window.sdl_window

run_engine::Engine a->IO ()
run_engine engine=FMA.allocaBytesAligned C.sdl_event_size C.sdl_event_alignment $ \sdl_event->case engine.timer of
    Off->loop_engine_off sdl_event engine
    On {}->loop_engine_on sdl_event engine

loop_engine_off::FP.Ptr ()->Engine a->IO ()
loop_engine_off sdl_event engine=do
    (switch,new_engine)<-run_request False engine
    value<-F.sdl_waitevent sdl_event
    if FMU.toBool value
        then do
            event_type<-C.sdl_event_type sdl_event
            loop_event switch event_type sdl_event new_engine
        else error "loop_engine_off: error 1"

loop_engine_off_a::FP.Ptr ()->Engine a->IO ()
loop_engine_off_a sdl_event engine=do
    value<-F.sdl_waitevent sdl_event
    if FMU.toBool value
        then do
            event_type<-C.sdl_event_type sdl_event
            loop_event False event_type sdl_event engine
        else error "loop_engine_off_a: error 1"

loop_engine_on::FP.Ptr ()->Engine a->IO ()
loop_engine_on sdl_event engine=do
    (switch,new_engine)<-run_request False engine
    value<-F.sdl_waitevent sdl_event
    if FMU.toBool value
        then do
            event_type<-C.sdl_event_type sdl_event
            if event_type==engine.event_number then let count=engine.count+1 in let time=engine.time+engine.timer.interval in loop_event_b (not switch) (Time {tick=count,time=time,interval=engine.timer.interval}) sdl_event (new_engine {count=count,time=time}) else loop_event (not switch) event_type sdl_event new_engine
        else error "loop_engine_on: error 1"

loop_engine_on_a::FP.Ptr ()->Engine a->IO ()
loop_engine_on_a sdl_event engine=do
    value<-F.sdl_waitevent sdl_event
    if FMU.toBool value
        then do
            event_type<-C.sdl_event_type sdl_event
            if event_type==engine.event_number then let count=engine.count+1 in let time=engine.time+engine.timer.interval in loop_event_b True (Time {tick=count,time=time,interval=engine.timer.interval}) sdl_event (engine {count=count,time=time}) else loop_event True event_type sdl_event engine
        else error "loop_engine_on_a: error 1"

loop_event::Bool->DW.Word32->FP.Ptr ()->Engine a->IO ()
loop_event on event_type sdl_event engine=case event_type of
    C.SDL_EVENT_QUIT->return ()
    C.SDL_EVENT_WINDOW_CLOSE_REQUESTED->do
        sdl_window_id<-C.sdl_windowevent_windowid sdl_event
        case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on sdl_event engine
            Just window_id->loop_event_b on (At {window_id=window_id,action=Close}) sdl_event engine
    C.SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED->do
        sdl_window_id<-C.sdl_windowevent_windowid sdl_event
        first_data<-C.sdl_windowevent_data1 sdl_event
        second_data<-C.sdl_windowevent_data2 sdl_event
        case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on sdl_event engine
            Just window_id->loop_event_b on (At {window_id=window_id,action=Resize {width=fromIntegral first_data,height=fromIntegral second_data}}) sdl_event engine
    C.SDL_EVENT_KEY_UP->do
        sdl_window_id<-C.sdl_keyboardevent_windowid sdl_event
        sdl_keycode<-C.sdl_keyboardevent_key sdl_event
        let change=to_key sdl_keycode in let maintain=DSet.delete change engine.key in let new_engine=engine {key=maintain} in case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on sdl_event engine
            Just window_id->loop_event_b on (At {window_id=window_id,action=Press {press=Press_up,change=change,maintain=maintain}}) sdl_event new_engine
    C.SDL_EVENT_KEY_DOWN->do
        sdl_window_id<-C.sdl_keyboardevent_windowid sdl_event
        sdl_keycode<-C.sdl_keyboardevent_key sdl_event
        let change=to_key sdl_keycode in let maintain=DSet.insert change engine.key in let new_engine=engine {key=maintain} in case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on sdl_event engine
            Just window_id->loop_event_b on (At {window_id=window_id,action=Press {press=Press_down,change=change,maintain=maintain}}) sdl_event new_engine
    _->loop_event_a on sdl_event engine

loop_event_a::Bool->FP.Ptr ()->Engine a->IO ()
loop_event_a on sdl_event engine=if on then loop_engine_on_a sdl_event engine else loop_engine_off_a sdl_event engine

loop_event_b::Bool->Event->FP.Ptr ()->Engine a->IO ()
loop_event_b on event sdl_event engine=let new_engine=run_event event engine in if on then loop_engine_on sdl_event new_engine else loop_engine_off sdl_event new_engine

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

run_request::Bool->Engine a->IO (Bool,Engine a)
run_request switch engine=case engine.request of
    DSeq.Empty->return (switch,engine)
    (request DSeq.:<| other_request)->do
        (new_engine,new_switch)<-do_request request (engine {request=other_request})
        run_request (switch/=new_switch) new_engine

run_event::Event->Engine a->Engine a
run_event event engine=case engine.main_id engine event of
    Nothing->engine
    Just active_id->run_event_a active_id event engine

run_event_a::Int->Event->Engine a->Engine a
run_event_a active_id event engine=let active=intmap_lookup active_id engine.active in let new_event=DF.foldl' (\this_event node_id->(intmap_lookup node_id engine.node).event_transform engine this_event) event active.ancestry in let new_engine=run_widget new_event (projection_lookup (engine.projection_strategy engine new_event) active.projection) engine in case active.next new_engine new_event of
    Nothing->new_engine
    Just new_active_id->run_event_a new_active_id event new_engine

run_widget::Event->Widget a->Engine a->Engine a
run_widget event widget engine=case widget of
    Trigger {trigger}->trigger event engine
    Io_trigger {io_trigger}->create_request (Io {io=io_trigger event}) engine
    _->error "run_widget: error 1"