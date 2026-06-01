{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Request where

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
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

create_request::Request a->Engine a->Engine a
create_request request engine=engine {request=engine.request DS.|> request}

do_request::Request a->Engine a->IO (Engine a)
do_request request engine=case request of
    Create_widget {father,widget_request,widget_id}->case widget_request of
        Trigger_request {}->create_active father widget_request widget_id engine
        Io_trigger_request {}->create_active father widget_request widget_id engine
        Geometry_request {}->create_bound father widget_request widget_id engine
    Remove_widget {widget_type,widget_id}->case widget_type of
        Active_widget->remove_active widget_id engine
        Free_widget->remove_free widget_id engine
        Bound_widget->remove_bound widget_id engine
    Create_node {father,event_transform,widget_transform,node_id}->return (create_node father event_transform widget_transform node_id engine)
    Remove_node {node_id}->remove_node node_id engine
    Create_window {window_id,title,width,height,window_flag}->FCS.withCString title $ \cstring->do
        sdl_window<-F.sdl_createwindow cstring width height (DF.foldl' (\word flag->word DB..|. from_window_flag flag) 0 window_flag)
        if sdl_window==FP.nullPtr then error "do_request: error 1" else do
            catch_error (F.sdl_claimwindowforgpudevice engine.device sdl_window)
            sdl_window_id<-F.sdl_getwindowid sdl_window
            triangle_graphics_pipeline<-create_triangle_graphics_pipeline sdl_window engine.device engine.vertex_shader engine.fragment_shader
            let (maybe_window,new_window)=DIM.insertLookupWithKey (\_ window _->window) window_id (Window {window_id=window_id,sdl_window_id=sdl_window_id,sdl_window=sdl_window,triangle_graphics_pipeline=triangle_graphics_pipeline,window_bound=DIS.empty}) engine.window in case maybe_window of
                Nothing->return (engine {window=new_window,window_map=map_insert sdl_window_id window_id engine.window_map})
                _->error "do_request: error 2"
    Remove_window {window_id}->remove_window window_id engine
    Clear_window {window_id,red,green,blue,alpha}->case DIM.lookup window_id engine.window of
        Nothing->error "do_request: error 3"
        Just window->do
            command_buffer<-F.sdl_acquiregpucommandbuffer engine.device
            FMA.alloca $ \ptr_texture->FMA.alloca $ \width->FMA.alloca $ \height->do
                catch_error (F.sdl_acquiregpuswapchaintexture command_buffer window.sdl_window ptr_texture width height)
                texture<-FS.peek ptr_texture
                CM.unless (texture==FP.nullPtr) $ FMU.with (C.SDL_GPUColorTargetInfo {texture=texture,clear_color=C.SDL_FColor {r=red,g=green,b=blue,a=alpha},load_op=C.sdl_gpu_loadop_clear,store_op=C.sdl_gpu_storeop_store}) $ \color_target_info->do
                    render_pass<-F.sdl_begingpurenderpass command_buffer color_target_info 1 FP.nullPtr
                    F.sdl_endgpurenderpass render_pass
            catch_error (F.sdl_submitgpucommandbuffer command_buffer)
            return engine
    Io {io}->io engine
    Render {bound_id}->case DIM.lookup bound_id engine.bound of
        Nothing->error "do_request: error 4"
        Just (Bound {window_id,ancestry,widget})->case DIM.lookup window_id engine.window of
            Nothing->error "do_request: error 5"
            Just window->case do_widget_transform ancestry engine request widget of
                Geometry {red,green,blue,alpha,geometry}->case geometry of
                    Triangle {first_x,first_y,second_x,second_y,third_x,third_y}->let device=engine.device in do
                        buffer<-create_vertex_buffer device [Vertex {red=red,green=green,blue=blue,alpha=alpha,x=first_x,y=first_y},Vertex {red=red,green=green,blue=blue,alpha=alpha,x=second_x,y=second_y},Vertex {red=red,green=green,blue=blue,alpha=alpha,x=third_x,y=third_y}]
                        command_buffer<-F.sdl_acquiregpucommandbuffer device
                        FMA.alloca $ \ptr_texture->FMA.alloca $ \width->FMA.alloca $ \height->do
                            catch_error (F.sdl_acquiregpuswapchaintexture command_buffer window.sdl_window ptr_texture width height)
                            texture<-FS.peek ptr_texture
                            CM.unless (texture==FP.nullPtr) $ FMU.with (C.SDL_GPUColorTargetInfo {texture=texture,clear_color=C.SDL_FColor {r=0,g=0,b=0,a=0},load_op=C.sdl_gpu_loadop_load,store_op=C.sdl_gpu_storeop_store}) $ \color_target_info->do
                                render_pass<-F.sdl_begingpurenderpass command_buffer color_target_info 1 FP.nullPtr
                                F.sdl_bindgpugraphicspipeline render_pass window.triangle_graphics_pipeline
                                FMU.with (C.SDL_GPUBufferBinding {buffer=buffer,offset=0}) $ \buffer_binding->F.sdl_bindgpuvertexbuffers render_pass 0 buffer_binding 1
                                F.sdl_drawgpuprimitives render_pass 3 1 0 0
                                F.sdl_endgpurenderpass render_pass
                        catch_error (F.sdl_submitgpucommandbuffer command_buffer)
                        F.sdl_releasegpubuffer device buffer
                        return engine
                _->error "do_request: error 6"

do_widget_transform::DS.Seq Int->Engine a->Request a->Widget a->Widget a
do_widget_transform ancestry engine request widget=DF.foldr (\node_id->do_widget_transform_a node_id engine.node engine request) widget ancestry

do_widget_transform_a::Int->DIM.IntMap (Node a)->Engine a->Request a->Widget a->Widget a
do_widget_transform_a node_id engine_node engine request widget=case DIM.lookup node_id engine_node of
    Nothing->error "do_widget_transform_a: error 1"
    Just node->node.widget_transform engine request widget

from_window_flag::Window_flag->DW.Word64
from_window_flag window_flag=case window_flag of
    Window_fullscreen->C.sdl_window_fullscreen
    Window_hidden->C.sdl_window_hidden
    Window_borderless->C.sdl_window_borderless
    Window_resizable->C.sdl_window_resizable