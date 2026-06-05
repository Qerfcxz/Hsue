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
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

create_request::Request a->Engine a->Engine a
create_request request engine=engine {request=engine.request DS.|> request}

do_request::Request a->Engine a->IO (Engine a,Bool)
do_request request engine=case request of
    Reset_timer {time}->if 0<time
        then case engine.timer of
            Nothing->do
                timer<-F.sdl_addtimer time engine.callback FP.nullPtr
                catch_zero timer
                return (engine {timer=Just timer},True)
            Just timer->do
                catch_false (F.sdl_removetimer timer)
                new_timer<-F.sdl_addtimer time engine.callback FP.nullPtr
                catch_zero new_timer
                return (engine {timer=Just new_timer},False)
        else error "do_request: error 1"
    Stop_timer->case engine.timer of
        Nothing->error "do_request: error 2"
        Just timer->do
            catch_false (F.sdl_removetimer timer)
            return (engine {timer=Nothing},True)
    Create_widget {father,widget_request,widget_id}->case widget_request of
        Trigger_request {}->return (create_active father widget_request widget_id engine,False)
        Io_trigger_request {}->return (create_active father widget_request widget_id engine,False)
        Collector_request {}->return (create_free father widget_request widget_id engine,False)
        Geometry_request {}->return (create_bound father widget_request widget_id engine,False)
    Remove_widget {widget_type,widget_id}->case widget_type of
        Active_widget->return (remove_active widget_id engine,False)
        Free_widget->return (remove_free widget_id engine,False)
        Bound_widget->return (remove_bound widget_id engine,False)
    Create_node {father,event_transform,widget_transform,node_id}->return (create_node father event_transform widget_transform node_id engine,False)
    Remove_node {node_id}->return (remove_node node_id engine,False)
    Create_window {window_id,title,width,height,window_flag}->DBS.useAsCString (DTE.encodeUtf8 title) $ \c_string->do
        sdl_window<-F.sdl_createwindow c_string width height (DF.foldl' (\word flag->word DB..|. from_window_flag flag) 0 window_flag)
        catch_null sdl_window
        catch_false (F.sdl_claimwindowforgpudevice engine.device sdl_window)
        sdl_window_id<-F.sdl_getwindowid sdl_window
        catch_zero sdl_window_id
        triangle_graphics_pipeline<-create_triangle_graphics_pipeline sdl_window engine.device engine.vertex_shader engine.fragment_shader
        let (maybe_window,new_window)=DIM.insertLookupWithKey (\_ window _->window) window_id (Window {window_id=window_id,sdl_window_id=sdl_window_id,sdl_window=sdl_window,triangle_graphics_pipeline=triangle_graphics_pipeline,window_bound=DIS.empty}) engine.window in case maybe_window of
            Nothing->return (engine {window=new_window,window_map=map_insert sdl_window_id window_id engine.window_map},False)
            _->error "do_request: error 3"
    Remove_window {window_id}->do
        new_engine<-remove_window window_id engine
        return (new_engine,False)
    Render {backup_path,window_id,submit_strategy}->case submit_strategy of
        Submit {consume}->let (free,widget)=consume_update_lookup_free_backup consume backup_path consume_widget engine.free in case widget of
            Collector {graph}->case for_submit graph of
                Graph {vertex,index}->let window=intmap_lookup window_id engine.window in do
                    command_buffer<-F.sdl_acquiregpucommandbuffer engine.device
                    catch_null command_buffer
                    maybe_index_length<-update_buffer engine.device command_buffer engine.vertex_buffer engine.index_buffer engine.vertex_size engine.index_size vertex index
                    FMA.alloca $ \pointer_texture->FMA.alloca $ \pointer_width->FMA.alloca $ \pointer_height->do
                        value<-F.sdl_acquiregpuswapchaintexture command_buffer window.sdl_window pointer_texture pointer_width pointer_height
                        CM.when (FMU.toBool value) $ do
                            texture<-FS.peek pointer_texture
                            CM.unless (texture==FP.nullPtr) $ FMU.with (C.SDL_GPUColorTargetInfo {texture=texture,clear_color=C.SDL_FColor {r=0,g=0,b=0,a=1},load_op=C.sdl_gpu_loadop_clear,store_op=C.sdl_gpu_storeop_store}) $ \color_target_info->do
                                render_pass<-F.sdl_begingpurenderpass command_buffer color_target_info 1 FP.nullPtr
                                catch_null render_pass
                                case maybe_index_length of
                                    Nothing->return ()
                                    Just index_length->do
                                        F.sdl_bindgpugraphicspipeline render_pass window.triangle_graphics_pipeline
                                        let size=4*FS.sizeOf (undefined::FCT.CFloat) in FMA.allocaBytesAligned size 16 $ \pointer->do
                                            width<-FS.peek pointer_width
                                            height<-FS.peek pointer_height
                                            FMU.fillBytes pointer 0 size
                                            FS.pokeElemOff pointer 0 (fromIntegral width::FCT.CFloat)
                                            FS.pokeElemOff pointer 1 (fromIntegral height::FCT.CFloat)
                                            F.sdl_pushgpuvertexuniformdata command_buffer 0 (FP.castPtr pointer) (fromIntegral size)
                                        FMU.with (C.SDL_GPUBufferBinding {buffer=engine.vertex_buffer,offset=0}) (\buffer_binding->F.sdl_bindgpuvertexbuffers render_pass 0 buffer_binding 1)
                                        FMU.with (C.SDL_GPUBufferBinding {buffer=engine.index_buffer,offset=0}) (\buffer_binding->F.sdl_bindgpuindexbuffer render_pass buffer_binding C.sdl_gpu_indexelementsize_32bit)
                                        F.sdl_drawgpuindexedprimitives render_pass index_length 1 0 0 0
                                F.sdl_endgpurenderpass render_pass
                    catch_false (F.sdl_submitgpucommandbuffer command_buffer)
                    return (engine {free=free},False)
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