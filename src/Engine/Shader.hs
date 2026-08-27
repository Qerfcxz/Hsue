{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Shader where

import Engine.Type
import Engine.Underlying
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Error.Type as ET
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

poke_vertex_attribute::ET.Has_call_stack=>FP.Ptr SDLI.SDL_GPUVertexAttribute->IO ()
poke_vertex_attribute vertex_attribute=do
    FS.pokeElemOff vertex_attribute 0 SDLI.SDL_GPUVertexAttribute {sdl_location=0,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_uint,sdl_offset=0}
    FS.pokeElemOff vertex_attribute 1 SDLI.SDL_GPUVertexAttribute {sdl_location=1,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float,sdl_offset=4}
    FS.pokeElemOff vertex_attribute 2 SDLI.SDL_GPUVertexAttribute {sdl_location=2,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float2,sdl_offset=8}
    FS.pokeElemOff vertex_attribute 3 SDLI.SDL_GPUVertexAttribute {sdl_location=3,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float2,sdl_offset=16}
    FS.pokeElemOff vertex_attribute 4 SDLI.SDL_GPUVertexAttribute {sdl_location=4,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float4,sdl_offset=24}

create_graphics_pipeline::ET.Has_call_stack=>FP.Ptr SDLT.SDL_Window->FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUShader->FP.Ptr SDLT.SDL_GPUShader->Blend_state->IO (FP.Ptr SDLT.SDL_GPUGraphicsPipeline)
create_graphics_pipeline window device vertex_shader fragment_shader blend_state=FMA.allocaArray 5 $ \vertex_attribute->do
    poke_vertex_attribute vertex_attribute
    FMU.with SDLI.SDL_GPUVertexBufferDescription {sdl_slot=0,sdl_pitch=40,sdl_input_rate=SDLI.sdl_gpu_vertexinputrate_vertex,sdl_instance_step_rate=0} $ \vertex_buffer_description->do
        format<-SDLF.sdl_get_gpu_swapchain_texture_format device window
        sdl_catch_zero format
        FMU.with SDLI.SDL_GPUColorTargetDescription {sdl_format=format,sdl_blend_state=from_blend_state blend_state} (\color_target_description->FMU.with SDLI.SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader=vertex_shader,sdl_fragment_shader=fragment_shader,sdl_vertex_input_state=SDLI.SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions=vertex_buffer_description,sdl_num_vertex_buffers=1,sdl_vertex_attributes=vertex_attribute,sdl_num_vertex_attributes=5},sdl_primitive_type=SDLI.sdl_gpu_primitivetype_trianglelist,sdl_target_info=SDLI.SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions=color_target_description,sdl_num_color_targets=1,sdl_has_depth_stencil_target=FMU.fromBool False}} (sdl_return_catch_null . SDLF.sdl_create_gpu_graphics_pipeline device))

create_canvas_graphics_pipeline::ET.Has_call_stack=>FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUShader->FP.Ptr SDLT.SDL_GPUShader->Blend_state->IO (FP.Ptr SDLT.SDL_GPUGraphicsPipeline)
create_canvas_graphics_pipeline device vertex_shader fragment_shader blend_state=FMA.allocaArray 5 $ \vertex_attribute->do
    poke_vertex_attribute vertex_attribute
    FMU.with SDLI.SDL_GPUVertexBufferDescription {sdl_slot=0,sdl_pitch=40,sdl_input_rate=SDLI.sdl_gpu_vertexinputrate_vertex,sdl_instance_step_rate=0} $ \vertex_buffer_description->FMU.with SDLI.SDL_GPUColorTargetDescription {sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_blend_state=from_blend_state blend_state} (\color_target_description->FMU.with SDLI.SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader=vertex_shader,sdl_fragment_shader=fragment_shader,sdl_vertex_input_state=SDLI.SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions=vertex_buffer_description,sdl_num_vertex_buffers=1,sdl_vertex_attributes=vertex_attribute,sdl_num_vertex_attributes=5},sdl_primitive_type=SDLI.sdl_gpu_primitivetype_trianglelist,sdl_target_info=SDLI.SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions=color_target_description,sdl_num_color_targets=1,sdl_has_depth_stencil_target=FMU.fromBool False}} (sdl_return_catch_null . SDLF.sdl_create_gpu_graphics_pipeline device))

load_shader::ET.Has_call_stack=>FP.Ptr SDLT.SDL_GPUDevice->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->String->IO (FP.Ptr SDLT.SDL_GPUShader)
load_shader device format stage num_sampler num_storage_buffer num_uniform_buffer path=do
    shader_code<-DBS.readFile path
    DBS.useAsCStringLen shader_code (\(code,code_size)->FCS.withCString "main" (\entrypoint->FMU.with (SDLI.SDL_GPUShaderCreateInfo {sdl_code_size=fromIntegral code_size,sdl_code=FP.castPtr code,sdl_entrypoint=entrypoint,sdl_format=format,sdl_stage=stage,sdl_num_samplers=num_sampler,sdl_num_storage_textures=0,sdl_num_storage_buffers=num_storage_buffer,sdl_num_uniform_buffers=num_uniform_buffer}) (sdl_return_catch_null . SDLF.sdl_create_gpu_shader device)))

from_blend_factor::ET.Has_call_stack=>Blend_factor->DW.Word32
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

from_blend_op::ET.Has_call_stack=>Blend_op->DW.Word32
from_blend_op blend_op=case blend_op of
    Blend_op_invalid->SDLI.sdl_gpu_blendop_invalid
    Blend_op_min->SDLI.sdl_gpu_blendop_min
    Blend_op_max->SDLI.sdl_gpu_blendop_max
    Blend_op_add->SDLI.sdl_gpu_blendop_add
    Blend_op_subtract->SDLI.sdl_gpu_blendop_subtract
    Blend_op_reverse_subtract->SDLI.sdl_gpu_blendop_reverse_subtract

from_color_component_flag::ET.Has_call_stack=>Color_component_flag->DW.Word8
from_color_component_flag color_component_flag=case color_component_flag of
    Color_component_r->SDLI.sdl_gpu_colorcomponent_r
    Color_component_g->SDLI.sdl_gpu_colorcomponent_g
    Color_component_b->SDLI.sdl_gpu_colorcomponent_b
    Color_component_a->SDLI.sdl_gpu_colorcomponent_a

from_blend_state::ET.Has_call_stack=>Blend_state->SDLI.SDL_GPUColorTargetBlendState
from_blend_state blend_state=case blend_state of
    Blend_state {src_color_blend_factor,dst_color_blend_factor,color_blend_op,src_alpha_blend_factor,dst_alpha_blend_factor,alpha_blend_op,color_write_mask,enable_blend,enable_color_write_mask}->SDLI.SDL_GPUColorTargetBlendState {sdl_src_color_blendfactor=from_blend_factor src_color_blend_factor,sdl_dst_color_blendfactor=from_blend_factor dst_color_blend_factor,sdl_color_blend_op=from_blend_op color_blend_op,sdl_src_alpha_blendfactor=from_blend_factor src_alpha_blend_factor,sdl_dst_alpha_blendfactor=from_blend_factor dst_alpha_blend_factor,sdl_alpha_blend_op=from_blend_op alpha_blend_op,sdl_color_write_mask=DF.foldl' (\this_color_write_mask color_component_flag->this_color_write_mask DB..|. from_color_component_flag color_component_flag) 0 color_write_mask,sdl_enable_blend=FMU.fromBool enable_blend,sdl_enable_color_write_mask=FMU.fromBool enable_color_write_mask}

from_filter::ET.Has_call_stack=>Filter->DW.Word32
from_filter this_filter=case this_filter of
    Filter_nearest->SDLI.sdl_gpu_filter_nearest
    Filter_linear->SDLI.sdl_gpu_filter_linear

from_sampler_mipmap_mode::ET.Has_call_stack=>Sampler_mipmap_mode->DW.Word32
from_sampler_mipmap_mode sampler_mipmap_mode=case sampler_mipmap_mode of
    Sampler_mipmap_mode_nearest->SDLI.sdl_gpu_samplermipmapmode_nearest
    Sampler_mipmap_mode_linear->SDLI.sdl_gpu_samplermipmapmode_linear

from_sampler_address_mode::ET.Has_call_stack=>Sampler_address_mode->DW.Word32
from_sampler_address_mode sampler_address_mode=case sampler_address_mode of
    Sampler_address_mode_repeat->SDLI.sdl_gpu_sampleraddressmode_repeat
    Sampler_address_mode_mirrored_repeat->SDLI.sdl_gpu_sampleraddressmode_mirrored_repeat
    Sampler_address_mode_clamp_to_edge->SDLI.sdl_gpu_sampleraddressmode_clamp_to_edge

from_sampler_create_info::ET.Has_call_stack=>Sampler_create_info->SDLI.SDL_GPUSamplerCreateInfo
from_sampler_create_info sampler_create_info=case sampler_create_info of
    Sampler_create_info {min_filter,mag_filter,mipmap_mode,address_mode_u,address_mode_v,address_mode_w}->
        SDLI.SDL_GPUSamplerCreateInfo {sdl_min_filter=from_filter min_filter,sdl_mag_filter=from_filter mag_filter,sdl_mipmap_mode=from_sampler_mipmap_mode mipmap_mode,sdl_address_mode_u=from_sampler_address_mode address_mode_u,sdl_address_mode_v=from_sampler_address_mode address_mode_v,sdl_address_mode_w=from_sampler_address_mode address_mode_w}

{-# INLINE from_blend_factor #-}
{-# INLINE from_blend_op #-}
{-# INLINE from_color_component_flag #-}
{-# INLINE from_blend_state #-}
{-# INLINE from_filter #-}
{-# INLINE from_sampler_mipmap_mode #-}
{-# INLINE from_sampler_address_mode #-}
{-# INLINE from_sampler_create_info #-}