{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Request where

import Engine.Atlas
import Engine.Collector
import Engine.Container
import Engine.Operation
import Engine.Projection
import Engine.Render
import Engine.Selector
import Engine.Shader
import Engine.Text
import Engine.Type
import Engine.Underlying
import Engine.Widget
import Engine.Window
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Error.Error as EE
import qualified Control.Monad.IO.Class as CMIOC
import qualified Control.Monad.Trans.State as CMTS
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Data.Word as DW
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP

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
    Create_window {window_id,title,window_width,window_height,red,green,blue,alpha,window_flag,blend_state}->DBS.useAsCString (DTE.encodeUtf8 title) $ \this_title->do
        window<-SDLF.sdl_create_window this_title window_width window_height (DF.foldl' (\this_window_flag single_window_flag->this_window_flag DB..|. from_window_flag single_window_flag) 0 window_flag)
        catch_null window
        catch_false (SDLF.sdl_claim_window_for_gpu_device engine.device window)
        catch_false (SDLF.sdl_set_gpu_swapchain_parameters engine.device window SDLI.sdl_gpu_swapchaincomposition_sdr SDLI.sdl_gpu_presentmode_mailbox)
        sdl_window_id<-SDLF.sdl_get_window_id window
        catch_zero sdl_window_id
        graphics_pipeline<-create_graphics_pipeline window engine.device engine.vertex_shader engine.fragment_shader blend_state
        let new_window_width=fromIntegral window_width in let new_window_height=fromIntegral window_height in return (engine {window=intmap_insert window_id (Window {window_id=window_id,sdl_window_id=sdl_window_id,sdl_window=window,graphics_pipeline=graphics_pipeline,design_width=new_window_width,design_height=new_window_height,adaptive_width=new_window_width,adaptive_height=new_window_height,width=new_window_width,height=new_window_height,red=red,green=green,blue=blue,alpha=alpha}) engine.window,window_map=map_insert sdl_window_id window_id engine.window_map},False)
    Remove_window {window_id}->do
        new_engine<-remove_window window_id engine
        return (new_engine,False)
    Create_canvas {canvas_width,canvas_height,maybe_canvas_id}->do
        texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=canvas_width,sdl_height=canvas_height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
        temporary_texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=canvas_width,sdl_height=canvas_height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
        case maybe_canvas_id of
            Nothing->return (engine {canvas=intmap_insert engine.canvas_id (Free_canvas {width=canvas_width,height=canvas_height,half_width=fromIntegral canvas_width/2,half_height=fromIntegral canvas_height/2,texture=texture,temporary_texture=temporary_texture}) engine.canvas,canvas_id=engine.canvas_id+1},False)
            Just canvas_id->return (engine {canvas=intmap_insert canvas_id (Free_canvas {width=canvas_width,height=canvas_height,half_width=fromIntegral canvas_width/2,half_height=fromIntegral canvas_height/2,texture=texture,temporary_texture=temporary_texture}) engine.canvas,canvas_id=max canvas_id engine.canvas_id+1},False)
    Remove_canvas {canvas_id}->let (canvas,single_canvas)=intmap_delete_lookup canvas_id engine.canvas in case single_canvas of
        Free_canvas {texture,temporary_texture}->do
            SDLF.sdl_release_gpu_texture engine.device texture
            SDLF.sdl_release_gpu_texture engine.device temporary_texture
            return (engine {canvas=canvas},False)
        Bound_canvas {texture,temporary_texture,leaf_id}->do
            SDLF.sdl_release_gpu_texture engine.device texture
            SDLF.sdl_release_gpu_texture engine.device temporary_texture
            return (engine {canvas=canvas,leaf=intmap_update leaf_id (update_projection_object lock_canvas_widget) engine.leaf},False)
    Create_shader {shader_id,stage,num_sampler,num_uniform_buffer,path}->do
        shader<-load_shader engine.device SDLI.sdl_gpu_shaderformat_dxil stage num_sampler 0 num_uniform_buffer path
        return (engine {shader=intmap_insert shader_id (Shader {sdl_shader=shader,pipeline_id=DIS.empty}) engine.shader},False)
    Remove_shader {shader_id}->let (shader,single_shader)=intmap_delete_lookup shader_id engine.shader in case single_shader of
        Shader {sdl_shader,pipeline_id}->if DIS.null pipeline_id
            then do
                SDLF.sdl_release_gpu_shader engine.device sdl_shader
                return (engine {shader=shader},False)
            else EE.quick_error "do_request" 2
    Create_pipeline {pipeline_id,maybe_vertex_shader_id,fragment_shader_id,blend_state}->case maybe_vertex_shader_id of
        Nothing->let (shader,fragment_shader)=intmap_update_lookup fragment_shader_id (insert_pipeline_id pipeline_id) engine.shader in do
            pipeline<-FMU.with SDLI.SDL_GPUColorTargetDescription {sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_blend_state=from_blend_state blend_state} $ \color_target_description->FMU.with SDLI.SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader=engine.default_shader,sdl_fragment_shader=fragment_shader.sdl_shader,sdl_vertex_input_state=SDLI.SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions=FP.nullPtr,sdl_num_vertex_buffers=0,sdl_vertex_attributes=FP.nullPtr,sdl_num_vertex_attributes=0},sdl_primitive_type=SDLI.sdl_gpu_primitivetype_trianglelist,sdl_target_info=SDLI.SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions=color_target_description,sdl_num_color_targets=1,sdl_has_depth_stencil_target=FMU.fromBool False}} (return_catch_null . SDLF.sdl_create_gpu_graphics_pipeline engine.device)
            return (engine {pipeline=intmap_insert pipeline_id (Default_pipeline {sdl_pipeline=pipeline,fragment_shader_id=fragment_shader_id}) engine.pipeline,shader=shader},False)
        Just vertex_shader_id->let (shader,vertex_shader)=intmap_update_lookup vertex_shader_id (insert_pipeline_id pipeline_id) engine.shader in let (new_shader,fragment_shader)=intmap_update_lookup fragment_shader_id (insert_pipeline_id pipeline_id) shader in do
            pipeline<-FMU.with SDLI.SDL_GPUColorTargetDescription {sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_blend_state=from_blend_state blend_state} $ \color_target_description->FMU.with SDLI.SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader=vertex_shader.sdl_shader,sdl_fragment_shader=fragment_shader.sdl_shader,sdl_vertex_input_state=SDLI.SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions=FP.nullPtr,sdl_num_vertex_buffers=0,sdl_vertex_attributes=FP.nullPtr,sdl_num_vertex_attributes=0},sdl_primitive_type=SDLI.sdl_gpu_primitivetype_trianglelist,sdl_target_info=SDLI.SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions=color_target_description,sdl_num_color_targets=1,sdl_has_depth_stencil_target=FMU.fromBool False}} (return_catch_null . SDLF.sdl_create_gpu_graphics_pipeline engine.device)
            return (engine {pipeline=intmap_insert pipeline_id (Pipeline {sdl_pipeline=pipeline,vertex_shader_id=vertex_shader_id,fragment_shader_id=fragment_shader_id}) engine.pipeline,shader=new_shader},False)
    Remove_pipeline {pipeline_id}->let (pipeline,single_pipeline)=intmap_delete_lookup pipeline_id engine.pipeline in case single_pipeline of
        Pipeline {sdl_pipeline,vertex_shader_id,fragment_shader_id}->do
            SDLF.sdl_release_gpu_graphics_pipeline engine.device sdl_pipeline
            return (engine {pipeline=pipeline,shader=intmap_update fragment_shader_id (delete_pipeline_id pipeline_id) (intmap_update vertex_shader_id (delete_pipeline_id pipeline_id) engine.shader)},False)
        Default_pipeline {sdl_pipeline,fragment_shader_id}->do
            SDLF.sdl_release_gpu_graphics_pipeline engine.device sdl_pipeline
            return (engine {pipeline=pipeline,shader=intmap_update fragment_shader_id (delete_pipeline_id pipeline_id) engine.shader},False)
    Create_sampler {sampler_id,sampler_create_info}->do
        sampler<-FMU.with (from_sampler_create_info sampler_create_info) (return_catch_null . SDLF.sdl_create_gpu_sampler engine.device)
        return (engine {sampler=intmap_insert sampler_id sampler engine.sampler},False)
    Remove_sampler {sampler_id}->let (sampler,single_sampler)=intmap_delete_lookup sampler_id engine.sampler in do
        SDLF.sdl_release_gpu_sampler engine.device single_sampler
        return (engine {sampler=sampler},False)
    Set_system_cursor {system_cursor}->do
        catch_false (SDLF.sdl_set_cursor (map_lookup system_cursor engine.system_cursor_map))
        return (engine,False)
    Clean_atlas->let initial_album=intmap_lookup engine.initial_album_id engine.album in let (atlas,left,down,right,up)=atlas_insert initial_album.width initial_album.height engine.padding (init_atlas engine.width engine.height) in do
        copy_texture engine.device initial_album.texture engine.texture left down initial_album.width initial_album.height
        return (engine {atlas=atlas,leaf=fmap (update_projection_object (all_selector_update lock_widget)) engine.leaf,font=DIM.empty,u=fromIntegral (left+right)*engine.reciprocal_width/2,v=fromIntegral (down+up)*engine.reciprocal_height/2},False)
    Unlock {leaf_id}->do
        (leaf,new_engine)<-CMTS.runStateT (intmap_functor_update leaf_id (functor_update_projection_object (all_selector_applicative_update (for_unlock leaf_id))) engine.leaf) engine
        return (new_engine {leaf=leaf},False)
    Load_charset {charset}->do
        new_engine<-update_font charset engine
        return (new_engine,False)
    Render {window_id,render_selector,projection_move,maybe_sampler_id}->let (new_engine,widget)=move_lookup projection_move engine in do
        command_buffer<-SDLF.sdl_acquire_gpu_command_buffer new_engine.device
        catch_null command_buffer
        let window=intmap_lookup window_id new_engine.window in let (vertex,index,parameter,draw_call)=for_submit (get_submit render_selector widget) in for_render window command_buffer (\texture->do_render new_engine window command_buffer texture maybe_sampler_id draw_call vertex index parameter)
        return (new_engine,False)
    Canvas_render {canvas_id,canvas_render_selector,projection_move,maybe_sampler_id}->do
        command_buffer<-SDLF.sdl_acquire_gpu_command_buffer engine.device
        case intmap_lookup canvas_id engine.canvas of
            Free_canvas {half_width,half_height,texture}->let (new_engine,widget)=move_lookup projection_move engine in do
                let (vertex,index,parameter,draw_call)=for_submit (get_submit canvas_render_selector widget) in do_render_canvas engine (half_width*2) (half_height*2) command_buffer texture maybe_sampler_id draw_call vertex index parameter
                catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
                return (new_engine,False)
            _->EE.quick_error "do_request" 3
    Canvas_widget_render {projection_path,canvas_widget_render_selector,projection_move,maybe_sampler_id}->do
        new_new_engine<-let (new_engine,widget)=move_lookup projection_move engine in selector_monad_action (for_canvas_widget_render maybe_sampler_id projection_path) canvas_widget_render_selector widget new_engine
        return (new_new_engine,False)
    Shader_canvas {uniform,canvas_id,pipeline_id,maybe_sampler_id}->case intmap_lookup canvas_id engine.canvas of
        Free_canvas {width,height,half_width,half_height,texture,temporary_texture}->do_shader_canvas (Free_canvas {width=width,height=height,half_width=half_width,half_height=half_height,texture=temporary_texture,temporary_texture=texture}) uniform canvas_id pipeline_id maybe_sampler_id texture temporary_texture engine
        Bound_canvas {texture,temporary_texture,leaf_id}->do_shader_canvas (Bound_canvas {texture=temporary_texture,temporary_texture=texture,leaf_id=leaf_id}) uniform canvas_id pipeline_id maybe_sampler_id texture temporary_texture engine
    Io {io}->do
        new_engine<-io engine
        return (new_engine,False)
    Custom_request {custom}->do
        new_engine<-custom_request custom engine
        return (new_engine,False)

from_system_cursor::System_cursor->DW.Word32
from_system_cursor system_cursor=case system_cursor of
    System_cursor_default->SDLI.sdl_system_cursor_default
    System_cursor_pointer->SDLI.sdl_system_cursor_pointer

for_unlock::Custom_widget d=>Int->Widget a b c d e->CMTS.StateT (Engine a b c d e) IO (Widget a b c d e)
for_unlock leaf_id widget=case widget of
    Visual {visual}->do
        engine<-CMTS.get
        (new_engine,new_visual)<-CMIOC.liftIO (for_unlock_visual leaf_id visual engine)
        CMTS.put new_engine
        return (Visual {visual=new_visual})
    Group_visual {arrange,collect_order,group_visual}->do
        engine<-CMTS.get
        (new_engine,new_group_visual)<-CMIOC.liftIO (intmap_monad_map (\_ visual this_engine->for_unlock_visual leaf_id visual this_engine) group_visual engine)
        CMTS.put new_engine
        return (Group_visual {arrange=arrange,collect_order=collect_order,group_visual=new_group_visual})
    Vector_visual {arrange,collect_order,size,vector_visual}->do
        engine<-CMTS.get
        new_vector_visual<-DVM.new size
        new_engine<-CMIOC.liftIO (vector_io_map (size-1) (\_ visual this_engine->for_unlock_visual leaf_id visual this_engine) vector_visual new_vector_visual engine)
        new_new_vector_visual<-DV.unsafeFreeze new_vector_visual
        CMTS.put new_engine
        return (Vector_visual {arrange=arrange,collect_order=collect_order,size=size,vector_visual=new_new_vector_visual})
    Custom_widget {custom}->do
        engine<-CMTS.get
        (new_engine,new_custom)<-CMIOC.liftIO (custom_widget_unlock custom engine)
        CMTS.put new_engine
        return (Custom_widget {custom=new_custom})
    _->return widget

for_unlock_visual::Int->Visual->Engine a b c d e->IO (Engine a b c d e,Visual)
for_unlock_visual leaf_id visual engine=case visual of
    Picture {arrange,path,locked}->if locked then create_picture arrange path engine else return (engine,visual)
    Atlas {arrange,clip_request,path,index,locked}->if locked then create_atlas arrange clip_request path index engine else return (engine,visual)
    Text {arrange,half_width,half_height,current_y,min_y,max_y,article,charset,locked}->if locked
        then do
            new_engine<-update_font charset engine
            return (new_engine,Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=current_y,min_y=min_y,max_y=max_y,article=fmap (fmap (update_article new_engine.font)) article,charset=charset,locked=False})
        else return (engine,visual)
    Canvas {arrange,canvas_width,canvas_height,half_width,half_height,canvas_id,locked}->if locked
        then do
            texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=canvas_width,sdl_height=canvas_height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
            temporary_texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=canvas_width,sdl_height=canvas_height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
            return (engine {canvas=intmap_insert canvas_id (Bound_canvas {texture=texture,temporary_texture=temporary_texture,leaf_id=leaf_id}) engine.canvas},Canvas {arrange=arrange,canvas_width=canvas_width,canvas_height=canvas_height,half_width=half_width,half_height=half_height,canvas_id=canvas_id,locked=False})
        else return (engine,visual)
    _->return (engine,visual)

update_article::DIM.IntMap Font->Row->Row
update_article font row=case row of
    Blank->Blank
    Row {row_core,x,y,width,min_down,max_up,min_descent,max_ascent}->Row {row_core=fmap (update_article_a font) row_core,x=x,y=y,width=width,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent}

update_article_a::DIM.IntMap Font->Character->Character
update_article_a font character=case character of
    Character {unicode,font_id,size,left,down,right,up,red,green,blue,alpha}->case intmap_lookup unicode (intmap_lookup font_id font).glyph of
        Glyph {min_u,min_v,max_u,max_v}->Character {unicode=unicode,font_id=font_id,size=size,left=left,down=down,right=right,up=up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,red=red,green=green,blue=blue,alpha=alpha}

do_shader_canvas::Canvas->Uniform->Int->Int->Maybe Int->FP.Ptr SDLT.SDL_GPUTexture->FP.Ptr SDLT.SDL_GPUTexture->Engine a b c d e->IO (Engine a b c d e,Bool)
do_shader_canvas canvas uniform canvas_id pipeline_id maybe_sampler_id texture temporary_texture engine=do
    command_buffer<-SDLF.sdl_acquire_gpu_command_buffer engine.device
    catch_null command_buffer
    FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=temporary_texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=0,sdl_g=0,sdl_b=0,sdl_a=0},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
        catch_null render_pass
        SDLF.sdl_bind_gpu_graphics_pipeline render_pass (get_sdl_pipeline (intmap_lookup pipeline_id engine.pipeline))
        FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=texture,sdl_sampler=maybe engine.default_sampler (\sampler_id->intmap_lookup sampler_id engine.sampler) maybe_sampler_id}) $ \texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1
        case uniform of
            Uniform {size,alignment,write}->FMA.allocaBytesAligned size alignment $ \ptr->do
                write ptr
                SDLF.sdl_push_gpu_fragment_uniform_data command_buffer 0 ptr (fromIntegral size)
        SDLF.sdl_draw_gpu_primitives render_pass 3 1 0 0
        SDLF.sdl_end_gpu_render_pass render_pass
        catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
        return (engine {canvas=DIM.insert canvas_id canvas engine.canvas},False)