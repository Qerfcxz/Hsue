{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Request where

import Engine.Atlas
import Engine.Collector
import Engine.Container
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
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.Functor.Compose as DFC
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
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
    Create_window {window_id,title,window_width,window_height,red,green,blue,alpha,window_flag}->DBS.useAsCString (DTE.encodeUtf8 title) $ \this_title->do
        window<-SDLF.sdl_create_window this_title window_width window_height (DF.foldl' (\flag this_window_flag->flag DB..|. from_window_flag this_window_flag) 0 window_flag)
        catch_null window
        catch_false (SDLF.sdl_claim_window_for_gpu_device engine.device window)
        catch_false (SDLF.sdl_set_gpu_swapchain_parameters engine.device window SDLI.sdl_gpu_swapchaincomposition_sdr SDLI.sdl_gpu_presentmode_mailbox)
        sdl_window_id<-SDLF.sdl_get_window_id window
        catch_zero sdl_window_id
        graphics_pipeline<-create_graphics_pipeline window engine.device engine.vertex_shader engine.fragment_shader
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
        Nothing->let (new_shader,fragment_shader)=intmap_update_lookup fragment_shader_id (insert_pipeline_id pipeline_id) engine.shader in do
            pipeline<-FMU.with SDLI.SDL_GPUColorTargetDescription {sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_blend_state=from_blend_state blend_state} $ \color_target_description->FMU.with SDLI.SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader=engine.default_shader,sdl_fragment_shader=fragment_shader.sdl_shader,sdl_vertex_input_state=SDLI.SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions=FP.nullPtr,sdl_num_vertex_buffers=0,sdl_vertex_attributes=FP.nullPtr,sdl_num_vertex_attributes=0},sdl_primitive_type=SDLI.sdl_gpu_primitivetype_trianglelist,sdl_target_info=SDLI.SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions=color_target_description,sdl_num_color_targets=1,sdl_has_depth_stencil_target=FMU.fromBool False}} (return_catch_null . SDLF.sdl_create_gpu_graphics_pipeline engine.device)
            return (engine {pipeline=intmap_insert pipeline_id (Default_pipeline {sdl_pipeline=pipeline,fragment_shader_id=fragment_shader_id}) engine.pipeline,shader=new_shader},False)
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
    Clean_atlas->let initial_album=intmap_lookup engine.initial_album_id engine.album in let (atlas,left,down,right,up)=atlas_insert initial_album.width initial_album.height engine.padding (init_atlas engine.width engine.height) in do
        copy_texture engine.device initial_album.texture engine.texture left down initial_album.width initial_album.height
        return (engine {atlas=atlas,leaf=fmap (update_projection_object (all_selector_update lock_widget)) engine.leaf,font=DIM.empty,u=fromIntegral (left+right)*engine.reciprocal_width/2,v=fromIntegral (down+up)*engine.reciprocal_height/2},False)
    Unlock {leaf_id}->do
        (new_engine,leaf)<-DFC.getCompose (intmap_functor_update leaf_id (functor_update_projection_object (\widget->DFC.Compose {getCompose=for_unlock leaf_id widget engine})) engine.leaf)
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
    Shader_canvas {canvas_id,pipeline_id,uniform}->case intmap_lookup canvas_id engine.canvas of
        Free_canvas {width,height,half_width,half_height,texture,temporary_texture}->do_shader_canvas (Free_canvas {width=width,height=height,half_width=half_width,half_height=half_height,texture=temporary_texture,temporary_texture=texture}) canvas_id pipeline_id uniform texture temporary_texture engine
        Bound_canvas {texture,temporary_texture,leaf_id}->do_shader_canvas (Bound_canvas {texture=temporary_texture,temporary_texture=texture,leaf_id=leaf_id}) canvas_id pipeline_id uniform texture temporary_texture engine
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

lock_canvas_widget::Widget a b c d e->Widget a b c d e
lock_canvas_widget widget=case widget of
    Visual {origin,matrix,red,green,blue,alpha,visual}->case visual of
        Canvas {width,height,half_width,half_height,canvas_id}->Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=Canvas {width=width,height=height,half_width=half_width,half_height=half_height,canvas_id=canvas_id,locked=True}}
        _->EE.quick_error "lock_canvas_widget" 0
    _->EE.quick_error "lock_canvas_widget" 1

from_blend_factor::Blend_factor->DW.Word32
from_blend_factor blend_factor=case blend_factor of
    Blend_factor_invalid->SDLI.sdl_gpu_blendfactor_invalid
    Blend_factor_zero->SDLI.sdl_gpu_blendfactor_zero
    Blend_factor_one->SDLI.sdl_gpu_blendfactor_one
    Blend_factor_constant_color->SDLI.sdl_gpu_blendfactor_constant_color
    Blend_factor_dst_color->SDLI.sdl_gpu_blendfactor_dst_color
    Blend_factor_src_color->SDLI.sdl_gpu_blendfactor_src_color
    Blend_factor_dst_alpha->SDLI.sdl_gpu_blendfactor_dst_alpha
    Blend_factor_src_alpha->SDLI.sdl_gpu_blendfactor_src_alpha
    Blend_factor_src_alpha_saturate->SDLI.sdl_gpu_blendfactor_src_alpha_saturate
    Blend_factor_one_minus_constant_color->SDLI.sdl_gpu_blendfactor_one_minus_constant_color
    Blend_factor_one_minus_dst_color->SDLI.sdl_gpu_blendfactor_one_minus_dst_color
    Blend_factor_one_minus_src_color->SDLI.sdl_gpu_blendfactor_one_minus_src_color
    Blend_factor_one_minus_dst_alpha->SDLI.sdl_gpu_blendfactor_one_minus_dst_alpha
    Blend_factor_one_minus_src_alpha->SDLI.sdl_gpu_blendfactor_one_minus_src_alpha

from_blend_op::Blend_op->DW.Word32
from_blend_op blend_op=case blend_op of
    Blend_op_invalid->SDLI.sdl_gpu_blendop_invalid
    Blend_op_min->SDLI.sdl_gpu_blendop_min
    Blend_op_max->SDLI.sdl_gpu_blendop_max
    Blend_op_add->SDLI.sdl_gpu_blendop_add
    Blend_op_subtract->SDLI.sdl_gpu_blendop_subtract
    Blend_op_reverse_subtract->SDLI.sdl_gpu_blendop_reverse_subtract

from_color_component_flag::Color_component_flag->DW.Word8
from_color_component_flag color_component_flag=case color_component_flag of
    Color_component_r->SDLI.sdl_gpu_colorcomponent_r
    Color_component_g->SDLI.sdl_gpu_colorcomponent_g
    Color_component_b->SDLI.sdl_gpu_colorcomponent_b
    Color_component_a->SDLI.sdl_gpu_colorcomponent_a

from_blend_state::Blend_state->SDLI.SDL_GPUColorTargetBlendState
from_blend_state blend_state=case blend_state of
    Blend_state {src_color_blend_factor,dst_color_blend_factor,color_blend_op,src_alpha_blend_factor,dst_alpha_blend_factor,alpha_blend_op,color_write_mask,enable_blend,enable_color_write_mask}->SDLI.SDL_GPUColorTargetBlendState {sdl_src_color_blendfactor=from_blend_factor src_color_blend_factor,sdl_dst_color_blendfactor=from_blend_factor dst_color_blend_factor,sdl_color_blend_op=from_blend_op color_blend_op,sdl_src_alpha_blendfactor=from_blend_factor src_alpha_blend_factor,sdl_dst_alpha_blendfactor=from_blend_factor dst_alpha_blend_factor,sdl_alpha_blend_op=from_blend_op alpha_blend_op,sdl_color_write_mask=DF.foldl' (\this_color_write_mask color_component_flag->this_color_write_mask DB..|. from_color_component_flag color_component_flag) 0 color_write_mask,sdl_enable_blend=FMU.fromBool enable_blend,sdl_enable_color_write_mask=FMU.fromBool enable_color_write_mask}

from_filter::Filter->DW.Word32
from_filter this_filter=case this_filter of
    Filter_nearest->SDLI.sdl_gpu_filter_nearest
    Filter_linear->SDLI.sdl_gpu_filter_linear

from_sampler_mipmap_mode::Sampler_mipmap_mode->DW.Word32
from_sampler_mipmap_mode sampler_mipmap_mode=case sampler_mipmap_mode of
    Sampler_mipmap_mode_nearest->SDLI.sdl_gpu_samplermipmapmode_nearest
    Sampler_mipmap_mode_linear->SDLI.sdl_gpu_samplermipmapmode_linear

from_sampler_address_mode::Sampler_address_mode->DW.Word32
from_sampler_address_mode sampler_address_mode=case sampler_address_mode of
    Sampler_address_mode_repeat->SDLI.sdl_gpu_sampleraddressmode_repeat
    Sampler_address_mode_mirrored_repeat->SDLI.sdl_gpu_sampleraddressmode_mirrored_repeat
    Sampler_address_mode_clamp_to_edge->SDLI.sdl_gpu_sampleraddressmode_clamp_to_edge

from_sampler_create_info::Sampler_create_info->SDLI.SDL_GPUSamplerCreateInfo
from_sampler_create_info sampler_create_info=case sampler_create_info of
    Sampler_create_info {min_filter,mag_filter,mipmap_mode,address_mode_u,address_mode_v,address_mode_w}->
        SDLI.SDL_GPUSamplerCreateInfo {sdl_min_filter=from_filter min_filter,sdl_mag_filter=from_filter mag_filter,sdl_mipmap_mode=from_sampler_mipmap_mode mipmap_mode,sdl_address_mode_u=from_sampler_address_mode address_mode_u,sdl_address_mode_v=from_sampler_address_mode address_mode_v,sdl_address_mode_w=from_sampler_address_mode address_mode_w}

lock_widget::Custom_widget d=>Widget a b c d e->Widget a b c d e
lock_widget widget=case widget of
    Visual {origin,matrix,red,green,blue,alpha,visual}->case visual of
        Picture {half_width,half_height,min_u,min_v,max_u,max_v,path}->Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=Picture {half_width=half_width,half_height=half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,path=path,locked=True}}
        Atlas {clip_request,path,clip,index}->Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=Atlas {clip_request=clip_request,path=path,clip=clip,index=index,locked=True}}
        _->widget
    Text {origin,matrix,half_width,half_height,y,max_y,article,charset}->Text {origin=origin,matrix=matrix,half_width=half_width,half_height=half_height,y=y,max_y=max_y,article=article,charset=charset,locked=True}
    Custom_widget {custom}->Custom_widget {custom=custom_widget_lock custom}
    _->widget

for_unlock::Custom_widget d=>Int->Widget a b c d e->Engine a b c d e->IO (Engine a b c d e,Widget a b c d e)
for_unlock leaf_id this_widget engine=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->do
        (new_engine,new_group_widget)<-DIM.foldlWithKey' (\action this_index widget->intmap_monad_action this_index (for_unlock leaf_id widget) action) (return (engine,DIM.empty)) group_widget
        return (new_engine,Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=new_group_widget})
    Widget_trigger {next,widget_trigger,widget}->do
        (new_engine,new_widget)<-for_unlock leaf_id widget engine
        return (new_engine,Widget_trigger {next=next,widget_trigger=widget_trigger,widget=new_widget})
    Widget_io_trigger {next,widget_io_trigger,widget}->do
        (new_engine,new_widget)<-for_unlock leaf_id widget engine
        return (new_engine,Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=new_widget})
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->do
        (new_engine,new_widget)<-for_unlock leaf_id widget engine
        return (new_engine,Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=new_widget})
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->do
        (new_engine,new_coroutine_state)<-DIM.foldlWithKey' (\action this_index single_coroutine_state->intmap_monad_action this_index (\this_engine->for_unlock_coroutine leaf_id this_engine single_coroutine_state) action) (return (engine,DIM.empty)) coroutine_state
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
        Canvas {width,height,half_width,half_height,canvas_id,locked}->if locked
            then do
                texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=width,sdl_height=height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
                temporary_texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=width,sdl_height=height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
                return (engine {canvas=intmap_insert canvas_id (Bound_canvas {texture=texture,temporary_texture=temporary_texture,leaf_id=leaf_id}) engine.canvas},Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=Canvas {width=width,height=height,half_width=half_width,half_height=half_height,canvas_id=canvas_id,locked=False}})
            else return (engine,this_widget)
        _->return (engine,this_widget)
    Text {origin,matrix,half_width,half_height,y,max_y,article,charset,locked}->if locked
        then do
            new_engine<-update_font charset engine
            return (new_engine,Text {origin=origin,matrix=matrix,half_width=half_width,half_height=half_height,y=y,max_y=max_y,article=fmap (fmap (update_article new_engine.font)) article,charset=charset,locked=False})
        else return (engine,this_widget)
    Custom_widget {custom}->do
        (new_engine,new_custom)<-custom_widget_unlock custom engine
        return (new_engine,Custom_widget {custom=new_custom})
    _->return (engine,this_widget)

for_unlock_coroutine::Custom_widget d=>Int->Engine a b c d e->Coroutine_state a b c d e->IO (Engine a b c d e,Coroutine_state a b c d e)
for_unlock_coroutine leaf_id engine coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->do
        (new_engine,new_widget)<-for_unlock leaf_id widget engine
        return (new_engine,Coroutine_state {widget=new_widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index})

update_article::DIM.IntMap Font->Row->Row
update_article font row=case row of
    Blank->Blank
    Row {row_core,x,y,width,min_down,max_up,min_descent,max_ascent}->Row {row_core=fmap (update_article_a font) row_core,x=x,y=y,width=width,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent}

update_article_a::DIM.IntMap Font->Character->Character
update_article_a font character=case character of
    Character {unicode,font_id,size,left,down,right,up,red,green,blue,alpha}->case intmap_lookup unicode (intmap_lookup font_id font).glyph of
        Glyph {min_u,min_v,max_u,max_v}->Character {unicode=unicode,font_id=font_id,size=size,left=left,down=down,right=right,up=up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,red=red,green=green,blue=blue,alpha=alpha}

do_shader_canvas::Canvas->Int->Int->Uniform->FP.Ptr SDLT.SDL_GPUTexture->FP.Ptr SDLT.SDL_GPUTexture->Engine a b c d e->IO (Engine a b c d e,Bool)
do_shader_canvas canvas canvas_id pipeline_id uniform texture temporary_texture engine=do
    command_buffer<-SDLF.sdl_acquire_gpu_command_buffer engine.device
    catch_null command_buffer
    FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=temporary_texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=0,sdl_g=0,sdl_b=0,sdl_a=0},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
        catch_null render_pass
        SDLF.sdl_bind_gpu_graphics_pipeline render_pass (get_sdl_pipeline (intmap_lookup pipeline_id engine.pipeline))
        FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=texture,sdl_sampler=engine.default_sampler}) $ \texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1
        case uniform of
            Uniform {size,alignment,write}->FMA.allocaBytesAligned size alignment $ \ptr->do
                write ptr
                SDLF.sdl_push_gpu_fragment_uniform_data command_buffer 0 ptr (fromIntegral size)
        SDLF.sdl_draw_gpu_primitives render_pass 3 1 0 0
        SDLF.sdl_end_gpu_render_pass render_pass
        catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
        return (engine {canvas=DIM.insert canvas_id canvas engine.canvas},False)

get_sdl_pipeline::Pipeline->FP.Ptr SDLT.SDL_GPUGraphicsPipeline
get_sdl_pipeline pipeline=case pipeline of
    Pipeline {sdl_pipeline}->sdl_pipeline
    Default_pipeline {sdl_pipeline}->sdl_pipeline