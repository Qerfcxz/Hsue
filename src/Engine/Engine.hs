{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Engine where

import Engine.Atlas
import Engine.Event
import Engine.Operation
import Engine.Shader
import Engine.Type
import Engine.Underlying
import Engine.Widget
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

create_engine::a->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Projection_strategy)->FCT.CInt->Int->Int->Int->Int->Int->Int->Int->Maybe DW.Word64->DW.Word64->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->Sampler_create_info->Blend_state->IO (Engine a b c d e)
create_engine custom main_id projection_strategy max_picture_size max_vertex_size max_index_size max_parameter_size initial_canvas_id initial_album_id initial_font_id count maybe_interval time padding width height font_size pixel_range sampler_create_info blend_state=do
    system_cursor_default<-SDLF.sdl_create_system_cursor SDLI.sdl_system_cursor_default
    catch_null system_cursor_default
    system_cursor_pointer<-SDLF.sdl_create_system_cursor SDLI.sdl_system_cursor_pointer
    catch_null system_cursor_pointer
    device<-SDLF.sdl_create_gpu_device SDLI.sdl_gpu_shaderformat_dxil (FMU.fromBool True) FP.nullPtr
    catch_null device
    default_shader<-load_shader device SDLI.sdl_gpu_shaderformat_dxil SDLI.sdl_gpu_shaderstage_vertex 0 0 0 "Default"
    vertex_shader<-load_shader device SDLI.sdl_gpu_shaderformat_dxil SDLI.sdl_gpu_shaderstage_vertex 0 1 1 "Vertex"
    fragment_shader<-load_shader device SDLI.sdl_gpu_shaderformat_dxil SDLI.sdl_gpu_shaderstage_fragment 1 0 0 "Fragment"
    canvas_graphics_pipeline<-create_canvas_graphics_pipeline device vertex_shader fragment_shader blend_state
    texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=width,sdl_height=height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture device)
    sampler<-FMU.with (from_sampler_create_info sampler_create_info) (return_catch_null . SDLF.sdl_create_gpu_sampler device)
    command_buffer<-SDLF.sdl_acquire_gpu_command_buffer device
    catch_null command_buffer
    FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=0,sdl_g=0,sdl_b=0,sdl_a=0},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
        catch_null render_pass
        SDLF.sdl_end_gpu_render_pass render_pass
    catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
    let create_buffer=return_catch_null . SDLF.sdl_create_gpu_buffer device
    let new_max_vertex_size=fromIntegral max_vertex_size
    let new_max_index_size=fromIntegral max_index_size
    let new_max_parameter_size=fromIntegral max_parameter_size
    vertex_buffer<-FMU.with (SDLI.SDL_GPUBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_bufferusage_vertex,sdl_size=new_max_vertex_size}) create_buffer
    index_buffer<-FMU.with (SDLI.SDL_GPUBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_bufferusage_index,sdl_size=new_max_index_size}) create_buffer
    parameter_buffer<-FMU.with (SDLI.SDL_GPUBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_bufferusage_graphics_storage_read,sdl_size=new_max_parameter_size}) create_buffer
    transfer_buffer<-FMU.with (SDLI.SDL_GPUTransferBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_transferbufferusage_upload,sdl_size=new_max_vertex_size+new_max_index_size+new_max_parameter_size}) (return_catch_null . SDLF.sdl_create_gpu_transfer_buffer device)
    picture_transfer_buffer<-FMU.with (SDLI.SDL_GPUTransferBufferCreateInfo {sdl_usage=SDLI.sdl_gpu_transferbufferusage_upload,sdl_size=fromIntegral max_picture_size}) (return_catch_null . SDLF.sdl_create_gpu_transfer_buffer device)
    event_number<-SDLF.sdl_register_events 2
    callback<-SDLF.wrapper $ \_ _ interval->do
        FMA.allocaBytesAligned SDLI.sdl_event_size SDLI.sdl_event_alignment $ \ptr->do
            FMU.fillBytes ptr 0 SDLI.sdl_event_size
            FS.poke (FP.castPtr ptr) event_number
            catch_false (SDLF.sdl_push_event ptr)
        return interval
    (new_texture,new_width,new_height)<-from_image device picture_transfer_buffer max_picture_size "White"
    let (atlas,left,down,right,up)=atlas_insert new_width new_height padding (init_atlas width height)
    copy_texture device new_texture texture left down new_width new_height
    let album_id=initial_album_id+1 in case maybe_interval of
        Nothing->let reciprocal_width=1/fromIntegral width in let reciprocal_height=1/fromIntegral height in return (Engine {custom=custom,main_id=main_id,projection_strategy=projection_strategy,callback=callback,atlas=atlas,canvas=DIM.empty,album=DIM.singleton initial_album_id (Album {width=new_width,height=new_height,texture=new_texture}),leaf=DIM.empty,node=DIM.empty,window=DIM.empty,font=DIM.empty,window_map=DM.empty,font_map=DM.empty,system_cursor_map=DM.insert System_cursor_pointer system_cursor_pointer (DM.singleton System_cursor_default system_cursor_default),request=DSeq.empty,key=DSet.empty,device=device,texture=texture,sampler=DIM.empty,default_sampler=sampler,canvas_graphics_pipeline=canvas_graphics_pipeline,pipeline=DIM.empty,shader=DIM.empty,default_shader=default_shader,vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_buffer=vertex_buffer,index_buffer=index_buffer,parameter_buffer=parameter_buffer,transfer_buffer=transfer_buffer,picture_transfer_buffer=picture_transfer_buffer,max_picture_size=max_picture_size,max_vertex_size=max_vertex_size,max_index_size=max_index_size,max_parameter_size=max_parameter_size,initial_canvas_id=initial_canvas_id,canvas_id=initial_canvas_id,initial_album_id=initial_album_id,album_id=album_id,initial_font_id=initial_font_id,font_id=initial_font_id,count=count,timer=Off,time=time,event_number=event_number,padding=padding,width=width,height=height,reciprocal_width=reciprocal_width,reciprocal_height=reciprocal_height,u=fromIntegral (left+right)*reciprocal_width/2,v=fromIntegral (down+up)*reciprocal_height/2,font_size=font_size,pixel_range=pixel_range})
        Just interval->if 0<interval
            then do
                timer_id<-SDLF.sdl_add_timer_ns interval callback FP.nullPtr
                catch_zero timer_id
                let reciprocal_width=1/fromIntegral width in let reciprocal_height=1/fromIntegral height in return (Engine {custom=custom,main_id=main_id,projection_strategy=projection_strategy,callback=callback,atlas=atlas,canvas=DIM.empty,album=DIM.singleton initial_album_id (Album {width=new_width,height=new_height,texture=new_texture}),leaf=DIM.empty,node=DIM.empty,window=DIM.empty,font=DIM.empty,window_map=DM.empty,font_map=DM.empty,system_cursor_map=DM.insert System_cursor_pointer system_cursor_pointer (DM.singleton System_cursor_default system_cursor_default),request=DSeq.empty,key=DSet.empty,device=device,texture=texture,sampler=DIM.empty,default_sampler=sampler,canvas_graphics_pipeline=canvas_graphics_pipeline,pipeline=DIM.empty,shader=DIM.empty,default_shader=default_shader,vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_buffer=vertex_buffer,index_buffer=index_buffer,parameter_buffer=parameter_buffer,transfer_buffer=transfer_buffer,picture_transfer_buffer=picture_transfer_buffer,max_picture_size=max_picture_size,max_vertex_size=max_vertex_size,max_index_size=max_index_size,max_parameter_size=max_parameter_size,initial_canvas_id=initial_canvas_id,canvas_id=initial_canvas_id,initial_album_id=initial_album_id,album_id=album_id,initial_font_id=initial_font_id,font_id=initial_font_id,count=count,timer=On {timer_id=timer_id,interval=interval},time=time,event_number=event_number,padding=padding,width=width,height=height,reciprocal_width=reciprocal_width,reciprocal_height=reciprocal_height,u=fromIntegral (left+right)*reciprocal_width/2,v=fromIntegral (down+up)*reciprocal_height/2,font_size=font_size,pixel_range=pixel_range})
            else EE.quick_error "create_engine" 0

clean_engine::Engine a b c d e->IO ()
clean_engine engine=do
    _<-DM.traverseWithKey (const SDLF.sdl_destroy_cursor) engine.system_cursor_map
    DF.traverse_ (clean_window engine.device) (DIM.elems engine.window)
    case engine.timer of
        Off->return ()
        On {timer_id}->catch_false (SDLF.sdl_remove_timer timer_id)
    DF.traverse_ (clean_canvas engine.device) (DIM.elems engine.canvas)
    DF.traverse_ (\album->SDLF.sdl_release_gpu_texture engine.device album.texture) (DIM.elems engine.album)
    FP.freeHaskellFunPtr engine.callback
    SDLF.sdl_release_gpu_texture engine.device engine.texture
    DF.traverse_ (SDLF.sdl_release_gpu_sampler engine.device) (DIM.elems engine.sampler)
    SDLF.sdl_release_gpu_sampler engine.device engine.default_sampler
    SDLF.sdl_release_gpu_buffer engine.device engine.vertex_buffer
    SDLF.sdl_release_gpu_buffer engine.device engine.index_buffer
    SDLF.sdl_release_gpu_buffer engine.device engine.parameter_buffer
    SDLF.sdl_release_gpu_transfer_buffer engine.device engine.transfer_buffer
    SDLF.sdl_release_gpu_transfer_buffer engine.device engine.picture_transfer_buffer
    SDLF.sdl_release_gpu_graphics_pipeline engine.device engine.canvas_graphics_pipeline
    DF.traverse_ (SDLF.sdl_release_gpu_graphics_pipeline engine.device . get_sdl_pipeline) (DIM.elems engine.pipeline)
    DF.traverse_ (\shader->SDLF.sdl_release_gpu_shader engine.device shader.sdl_shader) (DIM.elems engine.shader)
    SDLF.sdl_release_gpu_shader engine.device engine.default_shader
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