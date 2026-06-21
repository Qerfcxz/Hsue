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

create_engine::a->(Engine a->Event->Maybe Int)->(Engine a->Event->Projection_strategy)->FCT.CInt->Int->Int->Int->Int->Int->Int->Maybe DW.Word64->DW.Word64->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->IO (Engine a)
create_engine state main_id projection_strategy picture_size vertex_size index_size parameter_size initial_album_id initial_font_id count maybe_interval time padding width height font_size pixel_range=do
    device<-F.sdl_creategpudevice C.sdl_gpu_shaderformat_dxil (FMU.fromBool True) FP.nullPtr
    catch_null device
    vertex_shader<-load_shader device C.sdl_gpu_shaderformat_dxil C.sdl_gpu_shaderstage_vertex 0 1 1 "Vertex"
    fragment_shader<-load_shader device C.sdl_gpu_shaderformat_dxil C.sdl_gpu_shaderstage_fragment 1 0 0 "Fragment"
    texture<-FMU.with (C.SDL_GPUTextureCreateInfo {sdl_type=C.sdl_gpu_texturetype_2d,sdl_format=C.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=C.sdl_gpu_textureusage_sampler DB..|. C.sdl_gpu_textureusage_color_target,sdl_width=width,sdl_height=height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=C.sdl_gpu_samplecount_1}) (return_catch_null . F.sdl_creategputexture device)
    sampler<-FMU.with (C.SDL_GPUSamplerCreateInfo {sdl_min_filter=C.sdl_gpu_filter_nearest,sdl_mag_filter=C.sdl_gpu_filter_nearest,sdl_mipmap_mode=C.sdl_gpu_samplermipmapmode_nearest,sdl_address_mode_u=C.sdl_gpu_sampleraddressmode_clamp_to_edge,sdl_address_mode_v=C.sdl_gpu_sampleraddressmode_clamp_to_edge,sdl_address_mode_w=C.sdl_gpu_sampleraddressmode_clamp_to_edge}) (return_catch_null . F.sdl_creategpusampler device)
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
    let new_parameter_size=fromIntegral parameter_size
    vertex_buffer<-FMU.with (C.SDL_GPUBufferCreateInfo {sdl_usage=C.sdl_gpu_bufferusage_vertex,sdl_size=new_vertex_size}) create_buffer
    index_buffer<-FMU.with (C.SDL_GPUBufferCreateInfo {sdl_usage=C.sdl_gpu_bufferusage_index,sdl_size=new_index_size}) create_buffer
    parameter_buffer<-FMU.with (C.SDL_GPUBufferCreateInfo {sdl_usage=C.sdl_gpu_bufferusage_graphics_storage_read,sdl_size=new_parameter_size}) create_buffer
    transfer_buffer<-FMU.with (C.SDL_GPUTransferBufferCreateInfo {sdl_usage=C.sdl_gpu_transferbufferusage_upload,sdl_size=new_vertex_size+new_index_size+new_parameter_size}) (return_catch_null . F.sdl_creategputransferbuffer device)
    picture_transfer_buffer<-FMU.with (C.SDL_GPUTransferBufferCreateInfo {sdl_usage=C.sdl_gpu_transferbufferusage_upload,sdl_size=fromIntegral picture_size}) (return_catch_null . F.sdl_creategputransferbuffer device)
    event_number<-F.sdl_registerevents 1
    callback<-F.wrapper $ \_ _ interval->do
        FMA.allocaBytesAligned C.sdl_event_size C.sdl_event_alignment $ \ptr->do
            FMU.fillBytes ptr 0 C.sdl_event_size
            FS.poke (FP.castPtr ptr) event_number
            catch_false (F.sdl_pushevent ptr)
        return interval
    (new_texture,new_width,new_height)<-load_texture device picture_transfer_buffer picture_size "White"
    let (new_atlas,left,down,right,up)=atlas_insert new_width new_height padding (init_atlas width height)
    copy_texture device new_texture texture left down new_width new_height
    let album_id=initial_album_id+1 in case maybe_interval of
        Nothing->let reciprocal_width=1/fromIntegral width in let reciprocal_height=1/fromIntegral height in return (Engine {state=state,main_id=main_id,projection_strategy=projection_strategy,callback=callback,atlas=new_atlas,album=DIM.singleton initial_album_id (Album {width=new_width,height=new_height,texture=new_texture}),leaf=DIM.empty,node=DIM.empty,window=DIM.empty,font=DIM.empty,window_map=DM.empty,font_map=DM.empty,request=DSeq.empty,key=DSet.empty,device=device,texture=texture,sampler=sampler,vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_buffer=vertex_buffer,index_buffer=index_buffer,parameter_buffer=parameter_buffer,transfer_buffer=transfer_buffer,picture_transfer_buffer=picture_transfer_buffer,picture_size=picture_size,vertex_size=vertex_size,index_size=index_size,parameter_size=parameter_size,initial_album_id=initial_album_id,album_id=album_id,initial_font_id=initial_font_id,font_id=initial_font_id,count=count,timer=Off,time=time,event_number=event_number,padding=padding,width=width,height=height,reciprocal_width=reciprocal_width,reciprocal_height=reciprocal_height,u=fromIntegral (left+right)*reciprocal_width/2,v=fromIntegral (down+up)*reciprocal_height/2,font_size=font_size,pixel_range=pixel_range})
        Just interval->if 0<interval
            then do
                timer_id<-F.sdl_addtimerns interval callback FP.nullPtr
                catch_zero timer_id
                let reciprocal_width=1/fromIntegral width in let reciprocal_height=1/fromIntegral height in return (Engine {state=state,main_id=main_id,projection_strategy=projection_strategy,callback=callback,atlas=new_atlas,album=DIM.singleton initial_album_id (Album {width=new_width,height=new_height,texture=new_texture}),leaf=DIM.empty,node=DIM.empty,window=DIM.empty,font=DIM.empty,window_map=DM.empty,font_map=DM.empty,request=DSeq.empty,key=DSet.empty,device=device,texture=texture,sampler=sampler,vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_buffer=vertex_buffer,index_buffer=index_buffer,parameter_buffer=parameter_buffer,transfer_buffer=transfer_buffer,picture_transfer_buffer=picture_transfer_buffer,picture_size=picture_size,vertex_size=vertex_size,index_size=index_size,parameter_size=parameter_size,initial_album_id=initial_album_id,album_id=album_id,initial_font_id=initial_font_id,font_id=initial_font_id,count=count,timer=On {timer_id=timer_id,interval=interval},time=time,event_number=event_number,padding=padding,width=width,height=height,reciprocal_width=reciprocal_width,reciprocal_height=reciprocal_height,u=fromIntegral (left+right)*reciprocal_width/2,v=fromIntegral (down+up)*reciprocal_height/2,font_size=font_size,pixel_range=pixel_range})
            else error "create_engine: error 1"

clean_engine::Engine a->IO ()
clean_engine engine=do
    DF.mapM_ (clean_window engine.device) (DIM.elems engine.window)
    case engine.timer of
        Off->return ()
        On {timer_id}->catch_false (F.sdl_removetimer timer_id)
    DF.mapM_ (\album->F.sdl_releasegputexture engine.device album.texture) (DIM.elems engine.album)
    FP.freeHaskellFunPtr engine.callback
    F.sdl_releasegputexture engine.device engine.texture
    F.sdl_releasegpusampler engine.device engine.sampler
    F.sdl_releasegpubuffer engine.device engine.vertex_buffer
    F.sdl_releasegpubuffer engine.device engine.index_buffer
    F.sdl_releasegpubuffer engine.device engine.parameter_buffer
    F.sdl_releasegputransferbuffer engine.device engine.transfer_buffer
    F.sdl_releasegputransferbuffer engine.device engine.picture_transfer_buffer
    F.sdl_releasegpushader engine.device engine.vertex_shader
    F.sdl_releasegpushader engine.device engine.fragment_shader
    F.sdl_destroygpudevice engine.device

clean_window::FP.Ptr T.SDL_GPUDevice->Window->IO ()
clean_window device window=do
    catch_false (F.sdl_waitforgpuidle device)
    F.sdl_releasewindowfromgpudevice device window.sdl_window
    F.sdl_releasegpugraphicspipeline device window.graphics_pipeline
    F.sdl_destroywindow window.sdl_window

run_engine::Engine a->IO ()
run_engine engine=FMA.allocaBytesAligned C.sdl_event_size C.sdl_event_alignment $ \sdl_event->case engine.timer of
    Off->loop_engine_off sdl_event engine
    On {}->loop_engine_on sdl_event engine

loop_engine_off::FP.Ptr ()->Engine a->IO ()
loop_engine_off sdl_event engine=do
    (new_engine,switch)<-run_request False engine
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
    (new_engine,switch)<-run_request False engine
    value<-F.sdl_waitevent sdl_event
    if FMU.toBool value
        then do
            event_type<-C.sdl_event_type sdl_event
            if event_type==engine.event_number then let count=engine.count+1 in let time=engine.time+get_interval engine.timer in loop_event_b (not switch) (Time {tick=count,time=time,interval=get_interval engine.timer}) sdl_event (new_engine {count=count,time=time}) else loop_event (not switch) event_type sdl_event new_engine
        else error "loop_engine_on: error 1"

loop_engine_on_a::FP.Ptr ()->Engine a->IO ()
loop_engine_on_a sdl_event engine=do
    value<-F.sdl_waitevent sdl_event
    if FMU.toBool value
        then do
            event_type<-C.sdl_event_type sdl_event
            if event_type==engine.event_number then let count=engine.count+1 in let time=engine.time+get_interval engine.timer in loop_event_b True (Time {tick=count,time=time,interval=get_interval engine.timer}) sdl_event (engine {count=count,time=time}) else loop_event True event_type sdl_event engine
        else error "loop_engine_on_a: error 1"

get_interval::Timer->DW.Word64
get_interval timer=case timer of
    On {interval}->interval
    _->error "get_interval: error 1"

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

run_request::Bool->Engine a->IO (Engine a,Bool)
run_request switch engine=case engine.request of
    DSeq.Empty->return (engine,switch)
    (request DSeq.:<| other_request)->do
        (new_engine,new_switch)<-do_request request (engine {request=other_request})
        run_request (switch/=new_switch) new_engine

run_event::Event->Engine a->Engine a
run_event event engine=case engine.main_id engine event of
    Nothing->engine
    Just leaf_id->run_event_a leaf_id event engine

run_event_a::Int->Event->Engine a->Engine a
run_event_a leaf_id event engine=let (next,update,leaf)=intmap_functor_update leaf_id (\projection->run_event_b projection event engine) engine.leaf in let new_engine=update (engine {leaf=leaf}) in case next new_engine of
    Nothing->new_engine
    Just new_leaf_id->run_event_a new_leaf_id event new_engine

run_event_b::Projection a->Event->Engine a->(Engine a->Maybe Int,Engine a->Engine a,Projection a)
run_event_b projection event engine=case projection of
    Without {ancestry_id}->let new_event=DF.foldl' (\this_event node_id->(intmap_lookup node_id engine.node).event_transform engine this_event) event ancestry_id in let (this_update,this_next,widget)=run_widget (lookup_projection_object projection) new_event engine in ((`this_next` new_event),this_update,insert_projection_object widget projection)
    With {ancestry_id}->let new_event=DF.foldl' (\this_event node_id->(intmap_lookup node_id engine.node).event_transform engine this_event) event ancestry_id in let (this_update,this_next,widget)=run_widget (lookup_projection_object projection) new_event engine in ((`this_next` new_event),this_update,insert_projection_object widget projection)

run_widget::Widget a->Event->Engine a->(Engine a->Engine a,Engine a->Event->Maybe Int,Widget a)
run_widget this_widget event engine=case this_widget of
    Double {which,first_widget,second_widget}->if which then let (update,next,new_first_widget)=run_widget first_widget event engine in (update,next,Double {which=which,first_widget=new_first_widget,second_widget=second_widget}) else let (update,next,new_second_widget)=run_widget second_widget event engine in (update,next,Double {which=which,first_widget=first_widget,second_widget=new_second_widget})
    Group {index,group_widget}->let (update,next,new_group_widget)=intmap_functor_update index (\widget->run_widget widget event engine) group_widget in (update,next,Group {index=index,group_widget=new_group_widget})
    Trigger {next,trigger}->(trigger event,next,this_widget)
    Io_trigger {next,io_trigger}->(create_request (Io {io=io_trigger event}),next,this_widget)
    Mix_trigger {next,mix_trigger,order}->(let (update,io_update)=mix_trigger event in if order then create_request (Io {io=io_update}) . update else update . create_request (Io {io=io_update}),next,this_widget)
    Widget_trigger {next,widget_trigger,widget}->let (update,new_widget)=widget_trigger widget event engine in (update,next,Widget_trigger {next=next,widget_trigger=widget_trigger,widget=new_widget})
    Widget_io_trigger {next,widget_io_trigger,widget}->let (update,new_widget)=widget_io_trigger widget event engine in (create_request (Io {io=update}),next,Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=new_widget})
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->let (update,io_update,new_widget)=widget_mix_trigger widget event engine in (if order then create_request (Io {io=io_update}) . update else update . create_request (Io {io=io_update}),next,Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=new_widget})
    _->error "run_widget: error 1"