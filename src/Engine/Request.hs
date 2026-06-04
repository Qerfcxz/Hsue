{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Request where

import Engine.Backup
import Engine.Collector
import Engine.Node
import Engine.Other
import Engine.Shader
import Engine.Type
import Engine.Widget
import Engine.Window
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified Control.Monad as CM
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

create_request::Request a->Engine a->Engine a
create_request request engine=engine {request=engine.request DS.|> request}

do_request::Request a->Engine a->IO (Engine a)
do_request request engine=case request of
    Reset_timer {time}->if 0<time
        then case engine.timer of
            Nothing->do
                timer<-F.sdl_addtimer time engine.callback FP.nullPtr
                return (engine {timer=Just timer})
            Just timer->do
                catch_error (F.sdl_removetimer timer)
                new_timer<-F.sdl_addtimer time engine.callback FP.nullPtr
                return (engine {timer=Just new_timer})
        else error "do_request: error 1"
    Stop_timer->case engine.timer of
        Nothing->error "do_request: error 2"
        Just timer->do
            catch_error (F.sdl_removetimer timer)
            return (engine {timer=Nothing})
    Create_widget {father,widget_request,widget_id}->case widget_request of
        Trigger_request {}->return (create_active father widget_request widget_id engine)
        Io_trigger_request {}->return (create_active father widget_request widget_id engine)
        Collector_request {}->return (create_free father widget_request widget_id engine)
        Geometry_request {}->return (create_bound father widget_request widget_id engine)
    Remove_widget {widget_type,widget_id}->case widget_type of
        Active_widget->return (remove_active widget_id engine)
        Free_widget->return (remove_free widget_id engine)
        Bound_widget->return (remove_bound widget_id engine)
    Create_node {father,event_transform,widget_transform,node_id}->return (create_node father event_transform widget_transform node_id engine)
    Remove_node {node_id}->return (remove_node node_id engine)
    Create_window {window_id,title,width,height,window_flag}->DBS.useAsCString (DTE.encodeUtf8 title) $ \cstring->do
        sdl_window<-F.sdl_createwindow cstring width height (DF.foldl' (\word flag->word DB..|. from_window_flag flag) 0 window_flag)
        if sdl_window==FP.nullPtr then error "do_request: error 3" else do
            catch_error (F.sdl_claimwindowforgpudevice engine.device sdl_window)
            sdl_window_id<-F.sdl_getwindowid sdl_window
            triangle_graphics_pipeline<-create_triangle_graphics_pipeline sdl_window engine.device engine.vertex_shader engine.fragment_shader
            let (maybe_window,new_window)=DIM.insertLookupWithKey (\_ window _->window) window_id (Window {window_id=window_id,sdl_window_id=sdl_window_id,sdl_window=sdl_window,triangle_graphics_pipeline=triangle_graphics_pipeline,window_bound=DIS.empty}) engine.window in case maybe_window of
                Nothing->return (engine {window=new_window,window_map=map_insert sdl_window_id window_id engine.window_map})
                _->error "do_request: error 4"
    Remove_window {window_id}->remove_window window_id engine
    Render {backup_path,window_id,submit_strategy}->case submit_strategy of
        Submit {consume}->let (new_free,new_widget)=whether_update_lookup_free_backup consume backup_path consume_widget engine.free in case new_widget of
            Collector {graph}->case for_submit graph of
                Graph {vertex,index}->let new_engine=engine {free=new_free} in if DS.null vertex||DS.null index then return new_engine else let window=intmap_lookup window_id engine.window in do
                    (buffer,vertex_size,index_length)<-create_buffer engine.device vertex index
                    command_buffer<-F.sdl_acquiregpucommandbuffer engine.device
                    CM.when (command_buffer==FP.nullPtr) (error "do_request: error 5")
                    FMA.alloca $ \ptr_texture->FMA.alloca $ \width->FMA.alloca $ \height->do
                        value<-F.sdl_acquiregpuswapchaintexture command_buffer window.sdl_window ptr_texture width height
                        CM.when (FMU.toBool value) $ do
                            texture<-FS.peek ptr_texture
                            CM.unless (texture==FP.nullPtr) $ FMU.with (C.SDL_GPUColorTargetInfo {texture=texture,clear_color=C.SDL_FColor {r=0,g=0,b=0,a=1},load_op=C.sdl_gpu_loadop_clear,store_op=C.sdl_gpu_storeop_store}) $ \color_target_info->do
                                render_pass<-F.sdl_begingpurenderpass command_buffer color_target_info 1 FP.nullPtr
                                F.sdl_bindgpugraphicspipeline render_pass window.triangle_graphics_pipeline
                                FMU.with (C.SDL_GPUBufferBinding {buffer=buffer,offset=0}) (\buffer_binding->F.sdl_bindgpuvertexbuffers render_pass 0 buffer_binding 1)
                                FMU.with (C.SDL_GPUBufferBinding {buffer=buffer,offset=vertex_size}) (\buffer_binding->F.sdl_bindgpuindexbuffer render_pass buffer_binding C.sdl_gpu_indexelementsize_32bit)
                                F.sdl_drawgpuindexedprimitives render_pass index_length 1 0 0 0
                                F.sdl_endgpurenderpass render_pass
                    catch_error (F.sdl_submitgpucommandbuffer command_buffer)
                    F.sdl_releasegpubuffer engine.device buffer
                    return new_engine
            _->error "do_request: error 6"
    Io {io}->io engine

from_window_flag::Window_flag->DW.Word64
from_window_flag window_flag=case window_flag of
    Window_fullscreen->C.sdl_window_fullscreen
    Window_hidden->C.sdl_window_hidden
    Window_borderless->C.sdl_window_borderless
    Window_resizable->C.sdl_window_resizable