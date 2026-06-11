{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Request where

import Engine.Atlas
import Engine.Collector
import Engine.Node
import Engine.Other
import Engine.Projection
import Engine.Shader
import Engine.Type
import Engine.Widget
import Engine.Window
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified SDL.Type as T
import qualified Control.Monad as CM
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

create_request::Request a->Engine a->Engine a
create_request request engine=engine {request=engine.request DS.|> request}

do_request::Request a->Engine a->IO (Engine a,Bool)
do_request request engine=case request of
    Reset_timer {interval}->if 0<interval
        then case engine.timer of
            Off->do
                timer_id<-F.sdl_addtimerns interval engine.callback FP.nullPtr
                catch_zero timer_id
                return (engine {timer=On {timer_id=timer_id,interval=interval}},True)
            On {timer_id}->do
                catch_false (F.sdl_removetimer timer_id)
                new_timer_id<-F.sdl_addtimerns interval engine.callback FP.nullPtr
                catch_zero new_timer_id
                return (engine {timer=On {timer_id=new_timer_id,interval=interval}},False)
        else error "do_request: error 1"
    Stop_timer->case engine.timer of
        On {timer_id}->do
            catch_false (F.sdl_removetimer timer_id)
            return (engine {timer=Off},True)
        _->error "do_request: error 2"
    Stop_timer_safe->case engine.timer of
        Off->return (engine,False)
        On {timer_id}->do
            catch_false (F.sdl_removetimer timer_id)
            return (engine {timer=Off},True)
    Create_widget {widget_id,father,widget_request}->case widget_request of
        Trigger_request {}->return (create_active widget_id father widget_request engine,False)
        Io_trigger_request {}->return (create_active widget_id father widget_request engine,False)
        Collector_request {}->do
            new_engine<-create_inactive widget_id father widget_request engine
            return (new_engine,False)
        Visual_request {}->do
            new_engine<-create_inactive widget_id father widget_request engine
            return (new_engine,False)
    Remove_widget {widget_id,widget_type}->if widget_type
        then do
            new_engine<-remove_active widget_id engine
            return (new_engine,False)
        else do
            new_engine<-remove_inactive widget_id engine
            return (new_engine,False)
    Create_node {node_id,father,event_transform,widget_transform}->return (create_node node_id father event_transform widget_transform engine,False)
    Remove_node {node_id}->do
        new_engine<-remove_node node_id engine
        return (new_engine,False)
    Create_window {window_id,title,width,height,red,green,blue,alpha,window_flag}->DBS.useAsCString (DTE.encodeUtf8 title) $ \this_title->do
        sdl_window<-F.sdl_createwindow this_title width height (DF.foldl' (\sdl_window_flag this_window_flag->sdl_window_flag DB..|. from_window_flag this_window_flag) 0 window_flag)
        catch_null sdl_window
        catch_false (F.sdl_claimwindowforgpudevice engine.device sdl_window)
        catch_false (F.sdl_setgpuswapchainparameters engine.device sdl_window C.sdl_gpu_swapchaincomposition_sdr C.sdl_gpu_presentmode_mailbox)
        sdl_window_id<-F.sdl_getwindowid sdl_window
        catch_zero sdl_window_id
        triangle_graphics_pipeline<-create_triangle_graphics_pipeline sdl_window engine.device engine.vertex_shader engine.fragment_shader
        let new_width=fromIntegral width in let new_height=fromIntegral height in let (maybe_window,new_window)=DIM.insertLookupWithKey (\_ window _->window) window_id (Window {window_id=window_id,sdl_window_id=sdl_window_id,sdl_window=sdl_window,triangle_graphics_pipeline=triangle_graphics_pipeline,design_width=new_width,design_height=new_height,adaptive_width=new_width,adaptive_height=new_height,red=red,green=green,blue=blue,alpha=alpha}) engine.window in case maybe_window of
            Nothing->return (engine {window=new_window,window_map=map_insert sdl_window_id window_id engine.window_map},False)
            _->error "do_request: error 3"
    Remove_window {window_id}->do
        new_engine<-remove_window window_id engine
        return (new_engine,False)
    Clean_atlas->let initial_album=intmap_lookup engine.initial_album_id engine.album in let (atlas,atlas_id,x,y,u,v)=atlas_insert_initial initial_album.width initial_album.height engine.padding (init_atlas engine.width engine.height engine.initial_atlas_id) in do
        copy_texture engine.device initial_album.texture engine.texture x y initial_album.width initial_album.height
        return (engine {atlas=atlas,atlas_id=atlas_id,u=u,v=v},False)
    Render {window_id,projection_move}->let (inactive,widget)=update_lookup_inactive_object projection_move consume_widget engine.inactive in case widget of
        Collector {submit}->let window=intmap_lookup window_id engine.window in do
            command_buffer<-F.sdl_acquiregpucommandbuffer engine.device
            catch_null command_buffer
            let (vertex,index,draw_call)=for_submit submit
            maybe_value<-update_buffer engine.device command_buffer engine.vertex_buffer engine.index_buffer engine.transfer_buffer engine.vertex_size engine.index_size vertex index
            case maybe_value of
                Nothing->FMA.alloca $ \ptr_texture->FMA.alloca $ \width->FMA.alloca $ \height->do
                    value<-F.sdl_acquiregpuswapchaintexture command_buffer window.sdl_window ptr_texture width height
                    CM.when (FMU.toBool value) $ do
                        texture<-FS.peek ptr_texture
                        CM.unless (texture==FP.nullPtr) $ FMU.with (C.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=C.SDL_FColor {sdl_r=window.red,sdl_g=window.green,sdl_b=window.blue,sdl_a=window.alpha},sdl_load_op=C.sdl_gpu_loadop_clear,sdl_store_op=C.sdl_gpu_storeop_store}) $ \color_target_info->do
                            render_pass<-F.sdl_begingpurenderpass command_buffer color_target_info 1 FP.nullPtr
                            catch_null render_pass
                            F.sdl_endgpurenderpass render_pass
                _->FMA.alloca $ \ptr_texture->FMA.alloca $ \width->FMA.alloca $ \height->do
                    value<-F.sdl_acquiregpuswapchaintexture command_buffer window.sdl_window ptr_texture width height
                    CM.when (FMU.toBool value) $ do
                        texture<-FS.peek ptr_texture
                        CM.unless (texture==FP.nullPtr) $ FMU.with (C.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=C.SDL_FColor {sdl_r=window.red,sdl_g=window.green,sdl_b=window.blue,sdl_a=window.alpha},sdl_load_op=C.sdl_gpu_loadop_clear,sdl_store_op=C.sdl_gpu_storeop_store}) $ \color_target_info->do
                            render_pass<-F.sdl_begingpurenderpass command_buffer color_target_info 1 FP.nullPtr
                            catch_null render_pass
                            F.sdl_bindgpugraphicspipeline render_pass window.triangle_graphics_pipeline
                            let size=4*FS.sizeOf (undefined::FCT.CFloat) in FMA.allocaBytesAligned size 16 $ \ptr->do
                                FMU.fillBytes ptr 0 size
                                FS.pokeElemOff ptr 0 window.adaptive_width
                                FS.pokeElemOff ptr 1 window.adaptive_height
                                F.sdl_pushgpuvertexuniformdata command_buffer 0 (FP.castPtr ptr) (fromIntegral size)
                            FMU.with (C.SDL_GPUBufferBinding {sdl_buffer=engine.vertex_buffer,sdl_offset=0}) (\buffer_binding->F.sdl_bindgpuvertexbuffers render_pass 0 buffer_binding 1)
                            FMU.with (C.SDL_GPUBufferBinding {sdl_buffer=engine.index_buffer,sdl_offset=0}) (\buffer_binding->F.sdl_bindgpuindexbuffer render_pass buffer_binding C.sdl_gpu_indexelementsize_32bit)
                            DF.mapM_ (do_render render_pass engine) draw_call
                            F.sdl_endgpurenderpass render_pass
            catch_false (F.sdl_submitgpucommandbuffer command_buffer)
            return (engine {inactive=inactive},False)
        _->error "do_request: error 4"
    Io {io}->do
        new_engine<-io engine
        return (new_engine,False)

from_window_flag::Window_flag->DW.Word64
from_window_flag window_flag=case window_flag of
    Window_fullscreen->C.sdl_window_fullscreen
    Window_hidden->C.sdl_window_hidden
    Window_borderless->C.sdl_window_borderless
    Window_resizable->C.sdl_window_resizable

do_render::FP.Ptr T.SDL_GPURenderPass->Engine a->(Maybe Int,DW.Word32,DW.Word32)->IO ()
do_render render_pass engine (maybe_album_id,index_length,index_offset)=case maybe_album_id of
    Nothing->do
        FMU.with (C.SDL_GPUTextureSamplerBinding {sdl_texture=engine.texture,sdl_sampler=engine.sampler}) (\texture_sampler_binding->F.sdl_bindgpufragmentsamplers render_pass 0 texture_sampler_binding 1)
        F.sdl_drawgpuindexedprimitives render_pass index_length 1 index_offset 0 0
    Just album_id->do
        FMU.with (C.SDL_GPUTextureSamplerBinding {sdl_texture=(intmap_lookup album_id engine.album).texture,sdl_sampler=engine.sampler}) (\texture_sampler_binding->F.sdl_bindgpufragmentsamplers render_pass 0 texture_sampler_binding 1)
        F.sdl_drawgpuindexedprimitives render_pass index_length 1 index_offset 0 0