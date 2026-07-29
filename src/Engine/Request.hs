{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Request where

import Engine.Atlas
import Engine.Collector
import Engine.Container
import Engine.Helper
import Engine.Leaf
import Engine.Node
import Engine.Projection
import Engine.Selector
import Engine.Shader
import Engine.Text
import Engine.Type
import Engine.Window
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Error.Error as EE
import qualified Control.Monad as CM
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.Functor.Compose as DFC
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

create_request::Request a b c d e->Engine a b c d e->Engine a b c d e
create_request request engine=engine {request=engine.request DS.|> request}

do_request::(Custom_request c,Custom_widget d,Custom_widget_request e)=>Request a b c d e->Engine a b c d e->IO (Engine a b c d e,Bool)
do_request request engine=case request of
    Reset_timer {interval}->if 0<interval
        then case engine.timer of
            Off->do
                timer_id<-SDLF.sdl_add_timer_ns interval engine.callback FP.nullPtr
                catch_zero timer_id
                return (engine {timer=On {timer_id=timer_id,interval=interval}},True)
            On {timer_id}->do
                catch_false (SDLF.sdl_remove_timer timer_id)
                new_timer_id<-SDLF.sdl_add_timer_ns interval engine.callback FP.nullPtr
                catch_zero new_timer_id
                return (engine {timer=On {timer_id=new_timer_id,interval=interval}},False)
        else EE.quick_error "do_request" 0
    Stop_timer->case engine.timer of
        On {timer_id}->do
            catch_false (SDLF.sdl_remove_timer timer_id)
            return (engine {timer=Off},True)
        _->EE.quick_error "do_request" 1
    Stop_timer_safe->case engine.timer of
        Off->return (engine,False)
        On {timer_id}->do
            catch_false (SDLF.sdl_remove_timer timer_id)
            return (engine {timer=Off},True)
    Create_widget {leaf_id,maybe_father_id,widget_request}->do
        new_engine<-create_leaf leaf_id maybe_father_id widget_request engine
        return (new_engine,False)
    Remove_widget {leaf_id}->do
        new_engine<-remove_leaf leaf_id engine
        return (new_engine,False)
    Create_node {node_id,maybe_father_id,event_transform,widget_transform}->return (create_node node_id maybe_father_id event_transform widget_transform engine,False)
    Remove_node {node_id}->do
        new_engine<-remove_node node_id engine
        return (new_engine,False)
    Create_window {window_id,title,width,height,red,green,blue,alpha,window_flag}->DBS.useAsCString (DTE.encodeUtf8 title) $ \this_title->do
        sdl_window<-SDLF.sdl_create_window this_title width height (DF.foldl' (\sdl_window_flag this_window_flag->sdl_window_flag DB..|. from_window_flag this_window_flag) 0 window_flag)
        catch_null sdl_window
        catch_false (SDLF.sdl_claim_window_for_gpu_device engine.device sdl_window)
        catch_false (SDLF.sdl_set_gpu_swapchain_parameters engine.device sdl_window SDLI.sdl_gpu_swapchaincomposition_sdr SDLI.sdl_gpu_presentmode_mailbox)
        sdl_window_id<-SDLF.sdl_get_window_id sdl_window
        catch_zero sdl_window_id
        graphics_pipeline<-create_graphics_pipeline sdl_window engine.device engine.vertex_shader engine.fragment_shader
        let new_width=fromIntegral width in let new_height=fromIntegral height in let window=intmap_insert window_id (Window {window_id=window_id,sdl_window_id=sdl_window_id,sdl_window=sdl_window,graphics_pipeline=graphics_pipeline,design_width=new_width,design_height=new_height,adaptive_width=new_width,adaptive_height=new_height,width=new_width,height=new_height,red=red,green=green,blue=blue,alpha=alpha}) engine.window in return (engine {window=window,window_map=map_insert sdl_window_id window_id engine.window_map},False)
    Remove_window {window_id}->do
        new_engine<-remove_window window_id engine
        return (new_engine,False)
    Clean_atlas->let initial_album=intmap_lookup engine.initial_album_id engine.album in let (atlas,left,down,right,up)=atlas_insert initial_album.width initial_album.height engine.padding (init_atlas engine.width engine.height) in do
        copy_texture engine.device initial_album.texture engine.texture left down initial_album.width initial_album.height
        return (engine {atlas=atlas,leaf=fmap (update_projection_object (all_selector_update lock_widget)) engine.leaf,font=DIM.empty,u=fromIntegral (left+right)*engine.reciprocal_width/2,v=fromIntegral (down+up)*engine.reciprocal_height/2},False)
    Unlock {leaf_id}->do
        (new_engine,leaf)<-DFC.getCompose (intmap_functor_update leaf_id (functor_update_projection_object (\widget->DFC.Compose {getCompose=for_unlock widget engine})) engine.leaf)
        return (new_engine {leaf=leaf},False)
    Load_charset {charset}->do
        new_engine<-update_font charset engine
        return (new_engine,False)
    Render {window_id,projection_move}->let (new_engine,widget)=move_lookup projection_move engine in let new_widget=widget_lookup widget in case new_widget of
        Collector {submit}->let window=intmap_lookup window_id engine.window in do
            command_buffer<-SDLF.sdl_acquire_gpu_command_buffer engine.device
            catch_null command_buffer
            let (vertex,index,parameter,draw_call)=for_submit submit
            for_render window command_buffer (\texture->do_render engine window command_buffer texture draw_call vertex index parameter)
            return (new_engine,False)
        _->EE.quick_error "do_request" 2
    Io {io}->do
        new_engine<-io engine
        return (new_engine,False)
    Custom_request {custom}->do
        new_engine<-custom_request custom engine
        return (new_engine,False)

from_window_flag::Window_flag->DW.Word64
from_window_flag window_flag=case window_flag of
    Window_fullscreen->SDLI.sdl_window_fullscreen
    Window_hidden->SDLI.sdl_window_hidden
    Window_borderless->SDLI.sdl_window_borderless
    Window_resizable->SDLI.sdl_window_resizable
    Window_always_on_top->SDLI.sdl_window_always_on_top

lock_widget::Custom_widget d=>Widget a b c d e->Widget a b c d e
lock_widget widget=case widget of
    Visual {origin,matrix,red,green,blue,alpha,visual}->case visual of
        Picture {width,height,min_u,min_v,max_u,max_v,path}->Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=Picture {width=width,height=height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,path=path,locked=True}}
        Atlas {clip_request,path,clip,index}->Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=Atlas {clip_request=clip_request,path=path,clip=clip,index=index,locked=True}}
        _->widget
    Text {origin,matrix,width,height,y,max_y,article,charset}->Text {origin=origin,matrix=matrix,width=width,height=height,y=y,max_y=max_y,article=article,charset=charset,locked=True}
    Custom_widget {custom}->Custom_widget {custom=custom_widget_lock custom}
    _->widget

for_unlock::Custom_widget d=>Widget a b c d e->Engine a b c d e->IO (Engine a b c d e,Widget a b c d e)
for_unlock this_widget engine=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->do
        (new_engine,new_group_widget)<-DIM.foldlWithKey' (\action this_index widget->intmap_monad_action this_index (for_unlock widget) action) (return (engine,DIM.empty)) group_widget
        return (new_engine,Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=new_group_widget})
    Widget_trigger {next,widget_trigger,widget}->do
        (new_engine,new_widget)<-for_unlock widget engine
        return (new_engine,Widget_trigger {next=next,widget_trigger=widget_trigger,widget=new_widget})
    Widget_io_trigger {next,widget_io_trigger,widget}->do
        (new_engine,new_widget)<-for_unlock widget engine
        return (new_engine,Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=new_widget})
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->do
        (new_engine,new_widget)<-for_unlock widget engine
        return (new_engine,Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=new_widget})
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->do
        (new_engine,new_coroutine_state)<-DIM.foldlWithKey' (\action this_index single_coroutine_state->intmap_monad_action this_index (`for_unlock_coroutine` single_coroutine_state) action) (return (engine,DIM.empty)) coroutine_state
        return (new_engine,Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
    Visual {origin,matrix,red,green,blue,alpha,visual}->case visual of
        Picture {path,locked}->if locked
            then do
                (new_engine,new_visual)<-create_picture path engine
                return (new_engine,Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=new_visual})
            else return (engine,this_widget)
        Atlas {clip_request,path,index,locked}->if locked
            then do
                (new_engine,new_visual)<-create_atlas index clip_request path engine
                return (new_engine,Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=new_visual})
            else return (engine,this_widget)
        _->return (engine,this_widget)
    Text {origin,matrix,width,height,y,max_y,article,charset,locked}->if locked
        then do
            new_engine<-update_font charset engine
            return (new_engine,Text {origin=origin,matrix=matrix,width=width,height=height,y=y,max_y=max_y,article=fmap (fmap (update_article new_engine.font)) article,charset=charset,locked=False})
        else return (engine,this_widget)
    Custom_widget {custom}->do
        (new_engine,new_custom)<-custom_widget_unlock custom engine
        return (new_engine,Custom_widget {custom=new_custom})
    _->return (engine,this_widget)

for_unlock_coroutine::Custom_widget d=>Engine a b c d e->Coroutine_state a b c d e->IO (Engine a b c d e,Coroutine_state a b c d e)
for_unlock_coroutine engine coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->do
        (new_engine,new_widget)<-for_unlock widget engine
        return (new_engine,Coroutine_state {widget=new_widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index})

update_article::DIM.IntMap Font->Row->Row
update_article font row=case row of
    Blank->Blank
    Row {row_core,x,y,width,min_down,max_up,min_descent,max_ascent}->Row {row_core=fmap (update_article_a font) row_core,x=x,y=y,width=width,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent}

update_article_a::DIM.IntMap Font->Character->Character
update_article_a font character=case character of
    Character {unicode,font_id,size,left,down,right,up,red,green,blue,alpha}->case intmap_lookup unicode (intmap_lookup font_id font).glyph of
        Glyph {min_u,min_v,max_u,max_v}->Character {unicode=unicode,font_id=font_id,size=size,left=left,down=down,right=right,up=up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,red=red,green=green,blue=blue,alpha=alpha}

for_render::Window->FP.Ptr SDLT.SDL_GPUCommandBuffer->(FP.Ptr SDLT.SDL_GPUTexture->IO ())->IO ()
for_render window command_buffer action=FMA.alloca $ \ptr_texture->FMA.alloca $ \width->FMA.alloca $ \height->do
    value<-SDLF.sdl_acquire_gpu_swapchain_texture command_buffer window.sdl_window ptr_texture width height
    if FMU.toBool value
        then do
            texture<-FS.peek ptr_texture
            if texture==FP.nullPtr then catch_false (SDLF.sdl_cancel_gpu_command_buffer command_buffer) else do
                action texture
                catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
        else catch_false (SDLF.sdl_cancel_gpu_command_buffer command_buffer)

do_render::Engine a b c d e->Window->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUTexture->DS.Seq (Maybe Int,DW.Word32,DW.Word32)->DS.Seq Vertex->DS.Seq DW.Word32->DS.Seq Parameter->IO ()
do_render engine window command_buffer texture draw_call vertex index parameter=do
    value<-update_buffer engine.device command_buffer engine.vertex_buffer engine.index_buffer engine.parameter_buffer engine.transfer_buffer engine.vertex_size engine.index_size engine.parameter_size vertex index parameter
    FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=window.red,sdl_g=window.green,sdl_b=window.blue,sdl_a=window.alpha},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
        catch_null render_pass
        CM.when value (do_render_a engine window command_buffer render_pass draw_call)
        SDLF.sdl_end_gpu_render_pass render_pass

do_render_a::Engine a b c d e->Window->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPURenderPass->DS.Seq (Maybe Int,DW.Word32,DW.Word32)->IO ()
do_render_a engine window command_buffer render_pass draw_call=do
    SDLF.sdl_bind_gpu_graphics_pipeline render_pass window.graphics_pipeline
    FMU.with engine.parameter_buffer (\parameter_buffer->SDLF.sdl_bind_gpu_vertex_storage_buffers render_pass 0 parameter_buffer 1)
    let size=4*FS.sizeOf (undefined::FCT.CFloat) in FMA.allocaBytesAligned size 16 $ \ptr->do
        FMU.fillBytes ptr 0 size
        FS.pokeElemOff ptr 0 window.adaptive_width
        FS.pokeElemOff ptr 1 window.adaptive_height
        FS.pokeElemOff ptr 2 engine.font_size
        FS.pokeElemOff ptr 3 engine.pixel_range
        SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) (fromIntegral size)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.vertex_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_vertex_buffers render_pass 0 buffer_binding 1)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.index_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_index_buffer render_pass buffer_binding SDLI.sdl_gpu_indexelementsize_32bit)
    DF.mapM_ (do_render_b render_pass engine) draw_call

do_render_b::FP.Ptr SDLT.SDL_GPURenderPass->Engine a b c d e->(Maybe Int,DW.Word32,DW.Word32)->IO ()
do_render_b render_pass engine (maybe_album_id,index_length,index_offset)=case maybe_album_id of
    Nothing->do
        FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=engine.texture,sdl_sampler=engine.sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
        SDLF.sdl_draw_gpu_indexed_primitives render_pass index_length 1 index_offset 0 0
    Just album_id->do
        FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=(intmap_lookup album_id engine.album).texture,sdl_sampler=engine.sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
        SDLF.sdl_draw_gpu_indexed_primitives render_pass index_length 1 index_offset 0 0