{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Engine where

import Engine.Atlas
import Engine.Container
import Engine.Helper
import Engine.Projection
import Engine.Request
import Engine.Selector
import Engine.Shader
import Engine.Type
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Error.Error as EE
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
    catch_false (FCS.withCString "SDL_TIMER_RESOLUTION" (FCS.withCString "1" . SDLF.sdl_set_hint))
    catch_false (SDLF.sdl_init SDLI.sdl_init_video)

quit_engine::IO ()
quit_engine=SDLF.sdl_quit

create_engine::a->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Projection_strategy)->FCT.CInt->Int->Int->Int->Int->Int->Int->Maybe DW.Word64->DW.Word64->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->IO (Engine a b c d e)
create_engine custom main_id projection_strategy picture_size vertex_size index_size parameter_size initial_album_id initial_font_id count maybe_interval time padding width height font_size pixel_range=do
    device<-SDLF.sdl_create_gpu_device SDLI.sdl_gpu_shaderformat_dxil (FMU.fromBool True) FP.nullPtr
    catch_null device
    vertex_shader<-load_shader device SDLI.sdl_gpu_shaderformat_dxil SDLI.sdl_gpu_shaderstage_vertex 0 1 1 "Vertex"
    fragment_shader<-load_shader device SDLI.sdl_gpu_shaderformat_dxil SDLI.sdl_gpu_shaderstage_fragment 1 0 0 "Fragment"
    texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=width,sdl_height=height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture device)
    sampler<-FMU.with (SDLI.SDL_GPUSamplerCreateInfo {sdl_min_filter=SDLI.sdl_gpu_filter_nearest,sdl_mag_filter=SDLI.sdl_gpu_filter_nearest,sdl_mipmap_mode=SDLI.sdl_gpu_samplermipmapmode_nearest,sdl_address_mode_u=SDLI.sdl_gpu_sampleraddressmode_clamp_to_edge,sdl_address_mode_v=SDLI.sdl_gpu_sampleraddressmode_clamp_to_edge,sdl_address_mode_w=SDLI.sdl_gpu_sampleraddressmode_clamp_to_edge}) (return_catch_null . SDLF.sdl_create_gpu_sampler device)
    command_buffer<-SDLF.sdl_acquire_gpu_command_buffer device
    catch_null command_buffer
    FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=0,sdl_g=0,sdl_b=0,sdl_a=0},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
        catch_null render_pass
        SDLF.sdl_end_gpu_render_pass render_pass
    catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
    let create_buffer=return_catch_null . SDLF.sdl_create_gpu_buffer device
    let new_vertex_size=fromIntegral vertex_size
    let new_index_size=fromIntegral index_size
    let new_parameter_size=fromIntegral parameter_size
    vertex_buffer<-FMU.with (SDLI.SDL_GPUBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_bufferusage_vertex,sdl_size=new_vertex_size}) create_buffer
    index_buffer<-FMU.with (SDLI.SDL_GPUBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_bufferusage_index,sdl_size=new_index_size}) create_buffer
    parameter_buffer<-FMU.with (SDLI.SDL_GPUBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_bufferusage_graphics_storage_read,sdl_size=new_parameter_size}) create_buffer
    transfer_buffer<-FMU.with (SDLI.SDL_GPUTransferBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_transferbufferusage_upload,sdl_size=new_vertex_size+new_index_size+new_parameter_size}) (return_catch_null . SDLF.sdl_create_gpu_transfer_buffer device)
    picture_transfer_buffer<-FMU.with (SDLI.SDL_GPUTransferBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_transferbufferusage_upload,sdl_size=fromIntegral picture_size}) (return_catch_null . SDLF.sdl_create_gpu_transfer_buffer device)
    event_number<-SDLF.sdl_register_events 2
    callback<-SDLF.wrapper $ \_ _ interval->do
        FMA.allocaBytesAligned SDLI.sdl_event_size SDLI.sdl_event_alignment $ \ptr->do
            FMU.fillBytes ptr 0 SDLI.sdl_event_size
            FS.poke (FP.castPtr ptr) event_number
            catch_false (SDLF.sdl_push_event ptr)
        return interval
    (new_texture,new_width,new_height)<-from_image device picture_transfer_buffer picture_size "White"
    let (atlas,left,down,right,up)=atlas_insert new_width new_height padding (init_atlas width height)
    copy_texture device new_texture texture left down new_width new_height
    let album_id=initial_album_id+1 in case maybe_interval of
        Nothing->let reciprocal_width=1/fromIntegral width in let reciprocal_height=1/fromIntegral height in return (Engine {custom=custom,main_id=main_id,projection_strategy=projection_strategy,callback=callback,atlas=atlas,album=DIM.singleton initial_album_id (Album {width=new_width,height=new_height,texture=new_texture}),leaf=DIM.empty,node=DIM.empty,window=DIM.empty,font=DIM.empty,window_map=DM.empty,font_map=DM.empty,request=DSeq.empty,key=DSet.empty,device=device,texture=texture,sampler=sampler,vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_buffer=vertex_buffer,index_buffer=index_buffer,parameter_buffer=parameter_buffer,transfer_buffer=transfer_buffer,picture_transfer_buffer=picture_transfer_buffer,picture_size=picture_size,vertex_size=vertex_size,index_size=index_size,parameter_size=parameter_size,initial_album_id=initial_album_id,album_id=album_id,initial_font_id=initial_font_id,font_id=initial_font_id,count=count,timer=Off,time=time,event_number=event_number,padding=padding,width=width,height=height,reciprocal_width=reciprocal_width,reciprocal_height=reciprocal_height,u=fromIntegral (left+right)*reciprocal_width/2,v=fromIntegral (down+up)*reciprocal_height/2,font_size=font_size,pixel_range=pixel_range})
        Just interval->if 0<interval
            then do
                timer_id<-SDLF.sdl_add_timer_ns interval callback FP.nullPtr
                catch_zero timer_id
                let reciprocal_width=1/fromIntegral width in let reciprocal_height=1/fromIntegral height in return (Engine {custom=custom,main_id=main_id,projection_strategy=projection_strategy,callback=callback,atlas=atlas,album=DIM.singleton initial_album_id (Album {width=new_width,height=new_height,texture=new_texture}),leaf=DIM.empty,node=DIM.empty,window=DIM.empty,font=DIM.empty,window_map=DM.empty,font_map=DM.empty,request=DSeq.empty,key=DSet.empty,device=device,texture=texture,sampler=sampler,vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_buffer=vertex_buffer,index_buffer=index_buffer,parameter_buffer=parameter_buffer,transfer_buffer=transfer_buffer,picture_transfer_buffer=picture_transfer_buffer,picture_size=picture_size,vertex_size=vertex_size,index_size=index_size,parameter_size=parameter_size,initial_album_id=initial_album_id,album_id=album_id,initial_font_id=initial_font_id,font_id=initial_font_id,count=count,timer=On {timer_id=timer_id,interval=interval},time=time,event_number=event_number,padding=padding,width=width,height=height,reciprocal_width=reciprocal_width,reciprocal_height=reciprocal_height,u=fromIntegral (left+right)*reciprocal_width/2,v=fromIntegral (down+up)*reciprocal_height/2,font_size=font_size,pixel_range=pixel_range})
            else EE.quick_error "create_engine" 0

clean_engine::Engine a b c d e->IO ()
clean_engine engine=do
    DF.traverse_ (clean_window engine.device) (DIM.elems engine.window)
    case engine.timer of
        Off->return ()
        On {timer_id}->catch_false (SDLF.sdl_remove_timer timer_id)
    DF.traverse_ (\album->SDLF.sdl_release_gpu_texture engine.device album.texture) (DIM.elems engine.album)
    FP.freeHaskellFunPtr engine.callback
    SDLF.sdl_release_gpu_texture engine.device engine.texture
    SDLF.sdl_release_gpu_sampler engine.device engine.sampler
    SDLF.sdl_release_gpu_buffer engine.device engine.vertex_buffer
    SDLF.sdl_release_gpu_buffer engine.device engine.index_buffer
    SDLF.sdl_release_gpu_buffer engine.device engine.parameter_buffer
    SDLF.sdl_release_gpu_transfer_buffer engine.device engine.transfer_buffer
    SDLF.sdl_release_gpu_transfer_buffer engine.device engine.picture_transfer_buffer
    SDLF.sdl_release_gpu_shader engine.device engine.vertex_shader
    SDLF.sdl_release_gpu_shader engine.device engine.fragment_shader
    SDLF.sdl_destroy_gpu_device engine.device

clean_window::FP.Ptr SDLT.SDL_GPUDevice->Window->IO ()
clean_window device window=do
    catch_false (SDLF.sdl_wait_for_gpu_idle device)
    SDLF.sdl_release_window_from_gpu_device device window.sdl_window
    SDLF.sdl_release_gpu_graphics_pipeline device window.graphics_pipeline
    SDLF.sdl_destroy_window window.sdl_window

run_engine::(Custom_request c,Custom_widget d,Custom_widget_request e)=>Engine a b c d e->IO ()
run_engine engine=FMA.allocaBytesAligned SDLI.sdl_event_size SDLI.sdl_event_alignment $ \sdl_event->case engine.timer of
    Off->loop_engine_off sdl_event engine
    On {}->loop_engine_on sdl_event engine

loop_engine_off::(Custom_request c,Custom_widget d,Custom_widget_request e)=>FP.Ptr ()->Engine a b c d e->IO ()
loop_engine_off sdl_event engine=do
    (new_engine,switch)<-run_request False engine
    value<-SDLF.sdl_wait_event sdl_event
    if FMU.toBool value
        then do
            event_type<-SDLI.sdl_event_type_peek sdl_event
            loop_event switch event_type sdl_event new_engine
        else EE.quick_error "loop_engine_off" 0

loop_engine_off_a::(Custom_request c,Custom_widget d,Custom_widget_request e)=>FP.Ptr ()->Engine a b c d e->IO ()
loop_engine_off_a sdl_event engine=do
    value<-SDLF.sdl_wait_event sdl_event
    if FMU.toBool value
        then do
            event_type<-SDLI.sdl_event_type_peek sdl_event
            loop_event False event_type sdl_event engine
        else EE.quick_error "loop_engine_off_a" 0

loop_engine_on::(Custom_request c,Custom_widget d,Custom_widget_request e)=>FP.Ptr ()->Engine a b c d e->IO ()
loop_engine_on sdl_event engine=do
    (new_engine,switch)<-run_request False engine
    value<-SDLF.sdl_wait_event sdl_event
    if FMU.toBool value
        then do
            event_type<-SDLI.sdl_event_type_peek sdl_event
            if event_type==engine.event_number then let count=engine.count+1 in let interval=get_interval engine.timer in let time=engine.time+interval in loop_event_b (not switch) (Time {tick=count,time=time,interval=interval}) sdl_event (new_engine {count=count,time=time}) else loop_event (not switch) event_type sdl_event new_engine
        else EE.quick_error "loop_engine_on" 0

loop_engine_on_a::(Custom_request c,Custom_widget d,Custom_widget_request e)=>FP.Ptr ()->Engine a b c d e->IO ()
loop_engine_on_a sdl_event engine=do
    value<-SDLF.sdl_wait_event sdl_event
    if FMU.toBool value
        then do
            event_type<-SDLI.sdl_event_type_peek sdl_event
            if event_type==engine.event_number then let count=engine.count+1 in let interval=get_interval engine.timer in let time=engine.time+interval in loop_event_b True (Time {tick=count,time=time,interval=interval}) sdl_event (engine {count=count,time=time}) else loop_event True event_type sdl_event engine
        else EE.quick_error "loop_engine_on_a" 0

get_interval::Timer->DW.Word64
get_interval timer=case timer of
    On {interval}->interval
    _->EE.quick_error "get_interval" 0

loop_event::(Custom_request c,Custom_widget d,Custom_widget_request e)=>Bool->DW.Word32->FP.Ptr ()->Engine a b c d e->IO ()
loop_event on event_type sdl_event engine=case event_type of
    SDLI.SDL_EVENT_QUIT->return ()
    SDLI.SDL_EVENT_WINDOW_CLOSE_REQUESTED->do
        sdl_window_id<-SDLI.sdl_windowevent_windowid_peek sdl_event
        case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on sdl_event engine
            Just window_id->loop_event_b on (At {window_id=window_id,action=Close}) sdl_event engine
    SDLI.SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED->do
        sdl_window_id<-SDLI.sdl_windowevent_windowid_peek sdl_event
        first_data<-SDLI.sdl_windowevent_data1_peek sdl_event
        second_data<-SDLI.sdl_windowevent_data2_peek sdl_event
        case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on sdl_event engine
            Just window_id->loop_event_b on (At {window_id=window_id,action=Resize {width=fromIntegral first_data,height=fromIntegral second_data}}) sdl_event engine
    SDLI.SDL_EVENT_KEY_UP->do
        sdl_window_id<-SDLI.sdl_keyboardevent_windowid_peek sdl_event
        sdl_keycode<-SDLI.sdl_keyboardevent_key_peek sdl_event
        let change=to_key sdl_keycode in let maintain=DSet.delete change engine.key in case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on sdl_event engine
            Just window_id->loop_event_b on (At {window_id=window_id,action=Press {press=Press_up,change=change,maintain=maintain}}) sdl_event (engine {key=maintain})
    SDLI.SDL_EVENT_KEY_DOWN->do
        sdl_window_id<-SDLI.sdl_keyboardevent_windowid_peek sdl_event
        sdl_keycode<-SDLI.sdl_keyboardevent_key_peek sdl_event
        let change=to_key sdl_keycode in let maintain=DSet.insert change engine.key in case DM.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on sdl_event engine
            Just window_id->loop_event_b on (At {window_id=window_id,action=Press {press=Press_down,change=change,maintain=maintain}}) sdl_event (engine {key=maintain})
    _->if event_type==engine.event_number+1
        then do
            custom<-pop_event sdl_event
            loop_event_b on (Custom_event {custom=custom}) sdl_event engine
        else loop_event_a on sdl_event engine

loop_event_a::(Custom_request c,Custom_widget d,Custom_widget_request e)=>Bool->FP.Ptr ()->Engine a b c d e->IO ()
loop_event_a on sdl_event engine=if on then loop_engine_on_a sdl_event engine else loop_engine_off_a sdl_event engine

loop_event_b::(Custom_request c,Custom_widget d,Custom_widget_request e)=>Bool->Event a->FP.Ptr ()->Engine b a c d e->IO ()
loop_event_b on event sdl_event engine=let new_engine=run_event event engine in if on then loop_engine_on sdl_event new_engine else loop_engine_off sdl_event new_engine

to_key::DW.Word32->Key
to_key key=case key of
    SDLI.SDLK_A->Key_a
    SDLI.SDLK_B->Key_b
    SDLI.SDLK_C->Key_c
    SDLI.SDLK_D->Key_d
    SDLI.SDLK_E->Key_e
    SDLI.SDLK_F->Key_f
    SDLI.SDLK_G->Key_g
    SDLI.SDLK_H->Key_h
    SDLI.SDLK_I->Key_i
    SDLI.SDLK_J->Key_j
    SDLI.SDLK_K->Key_k
    SDLI.SDLK_L->Key_l
    SDLI.SDLK_M->Key_m
    SDLI.SDLK_N->Key_n
    SDLI.SDLK_O->Key_o
    SDLI.SDLK_P->Key_p
    SDLI.SDLK_Q->Key_q
    SDLI.SDLK_R->Key_r
    SDLI.SDLK_S->Key_s
    SDLI.SDLK_T->Key_t
    SDLI.SDLK_U->Key_u
    SDLI.SDLK_V->Key_v
    SDLI.SDLK_W->Key_w
    SDLI.SDLK_X->Key_x
    SDLI.SDLK_Y->Key_y
    SDLI.SDLK_Z->Key_z
    _->Key_unknown

run_request::(Custom_request c,Custom_widget d,Custom_widget_request e)=>Bool->Engine a b c d e->IO (Engine a b c d e,Bool)
run_request switch engine=case engine.request of
    DSeq.Empty->return (engine,switch)
    (request DSeq.:<| other_request)->do
        (new_engine,new_switch)<-do_request request (engine {request=other_request})
        run_request (switch/=new_switch) new_engine

run_event::Custom_widget d=>Event a->Engine b a c d e->Engine b a c d e
run_event event engine=case engine.main_id event engine of
    Nothing->engine
    Just leaf_id->run_event_a leaf_id event engine

run_event_a::Custom_widget d=>Int->Event a->Engine b a c d e->Engine b a c d e
run_event_a leaf_id event engine=case intmap_functor_update leaf_id (run_event_b event engine) engine.leaf of
    Event_result {first_value,update,second_value}->let new_engine=update (engine {leaf=second_value}) in case first_value new_engine of
        Nothing->new_engine
        Just new_leaf_id->run_event_a new_leaf_id event new_engine

run_event_b::Custom_widget d=>Event a->Engine b a c d e->Projection b a c d e->Event_result b a c d e (Engine b a c d e->Maybe Int) (Projection b a c d e)
run_event_b event engine projection=case projection of
    Without {ancestry_id}->let new_event=DF.foldl' (\this_event node_id->(intmap_lookup node_id engine.node).event_transform engine this_event) event ancestry_id in let event_result=trigger_selector_applicative_update True (run_widget new_event engine) (lookup_projection_object projection) in run_event_c new_event (`insert_projection_object` projection) event_result
    With {ancestry_id}->let new_event=DF.foldl' (\this_event node_id->(intmap_lookup node_id engine.node).event_transform engine this_event) event ancestry_id in let event_result=trigger_selector_applicative_update True (run_widget new_event engine) (lookup_projection_object projection) in run_event_c new_event (`insert_projection_object` projection) event_result

run_event_c::Event a->(Widget b a c d e->Projection b a c d e)->Event_result b a c d e (Event a->Engine b a c d e->Maybe Int) (Widget b a c d e)->Event_result b a c d e (Engine b a c d e->Maybe Int) (Projection b a c d e)
run_event_c event transform event_result=case event_result of
    Event_result {first_value,update,second_value}->Event_result {first_value=first_value event,update=update,second_value=transform second_value}

run_widget::Custom_widget d=>Event a->Engine b a c d e->Widget b a c d e->Event_result b a c d e (Event a->Engine b a c d e->Maybe Int) (Widget b a c d e)
run_widget event engine this_widget=case this_widget of
    Trigger {next,trigger}->Event_result {first_value=next,update=trigger event,second_value=this_widget}
    Io_trigger {next,io_trigger}->Event_result {first_value=next,update=create_request (Io {io=io_trigger event}),second_value=this_widget}
    Mix_trigger {next,mix_trigger,order}->Event_result {first_value=next,update=let (update,io_update)=mix_trigger event in if order then create_request (Io {io=io_update}) . update else update . create_request (Io {io=io_update}),second_value=this_widget}
    Widget_trigger {next,widget_trigger,widget}->let (new_widget,update)=widget_trigger event engine widget in Event_result {first_value=next,update=update,second_value=Widget_trigger {next=next,widget_trigger=widget_trigger,widget=new_widget}}
    Widget_io_trigger {next,widget_io_trigger,widget}->let (new_widget,update)=widget_io_trigger event engine widget in Event_result {first_value=next,update=create_request (Io {io=update}),second_value=Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=new_widget}}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->let (new_widget,update,io_update)=widget_mix_trigger event engine widget in Event_result {first_value=next,update=if order then create_request (Io {io=io_update}) . update else update . create_request (Io {io=io_update}),second_value=Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=new_widget}}
    Custom_widget {custom}->let (new_custom,update,next)=custom_widget_run event engine custom in Event_result {first_value=next,update=update,second_value=Custom_widget {custom=new_custom}}
    _->EE.quick_error "run_widget" 0