{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

module SDL.Include where

#include <SDL3/SDL.h>
#include <SDL3_image/SDL_image.h>

import SDL.Type
import Error.Error
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data SDL_FColor=SDL_FColor {sdl_r::CFloat,sdl_g::CFloat,sdl_b::CFloat,sdl_a::CFloat}

instance Storable SDL_FColor where
    sizeOf=sdl_f_color_size_of
    alignment=sdl_f_color_alignment
    peek=sdl_f_color_peek
    poke=sdl_f_color_poke

sdl_f_color_size_of::Num a=>SDL_FColor->a
sdl_f_color_size_of _=(#size SDL_FColor)

sdl_f_color_alignment::Num a=>SDL_FColor->a
sdl_f_color_alignment _=(#alignment SDL_FColor)

sdl_f_color_peek::Ptr SDL_FColor->IO SDL_FColor
sdl_f_color_peek _=quick_error "sdl_f_color_peek" 0

sdl_f_color_poke::Ptr SDL_FColor->SDL_FColor->IO ()
sdl_f_color_poke ptr f_color=case f_color of
    SDL_FColor {sdl_r,sdl_g,sdl_b,sdl_a}->do
        fillBytes ptr 0 (#size SDL_FColor)
        (#poke SDL_FColor,r) ptr sdl_r
        (#poke SDL_FColor,g) ptr sdl_g
        (#poke SDL_FColor,b) ptr sdl_b
        (#poke SDL_FColor,a) ptr sdl_a

data SDL_GPUBufferBinding=SDL_GPUBufferBinding {sdl_buffer::Ptr SDL_GPUBuffer,sdl_offset::Word32}

instance Storable SDL_GPUBufferBinding where
    sizeOf=sdl_gpu_buffer_binding_size_of
    alignment=sdl_gpu_buffer_binding_alignment
    peek=sdl_gpu_buffer_binding_peek
    poke=sdl_gpu_buffer_binding_poke

sdl_gpu_buffer_binding_size_of::Num a=>SDL_GPUBufferBinding->a
sdl_gpu_buffer_binding_size_of _=(#size SDL_GPUBufferBinding)

sdl_gpu_buffer_binding_alignment::Num a=>SDL_GPUBufferBinding->a
sdl_gpu_buffer_binding_alignment _=(#alignment SDL_GPUBufferBinding)

sdl_gpu_buffer_binding_peek::Ptr SDL_GPUBufferBinding->IO SDL_GPUBufferBinding
sdl_gpu_buffer_binding_peek _=quick_error "sdl_gpu_buffer_binding_peek" 0

sdl_gpu_buffer_binding_poke::Ptr SDL_GPUBufferBinding->SDL_GPUBufferBinding->IO ()
sdl_gpu_buffer_binding_poke ptr buffer_binding=case buffer_binding of
    SDL_GPUBufferBinding {sdl_buffer,sdl_offset}->do
        fillBytes ptr 0 (#size SDL_GPUBufferBinding)
        (#poke SDL_GPUBufferBinding,buffer) ptr sdl_buffer
        (#poke SDL_GPUBufferBinding,offset) ptr sdl_offset

data SDL_GPUColorTargetInfo=SDL_GPUColorTargetInfo {sdl_texture::Ptr SDL_GPUTexture,sdl_clear_color::SDL_FColor,sdl_load_op::Word32,sdl_store_op::Word32}

instance Storable SDL_GPUColorTargetInfo where
    sizeOf=sdl_gpu_color_target_info_size_of
    alignment=sdl_gpu_color_target_info_alignment
    peek=sdl_gpu_color_target_info_peek
    poke=sdl_gpu_color_target_info_poke

sdl_gpu_color_target_info_size_of::Num a=>SDL_GPUColorTargetInfo->a
sdl_gpu_color_target_info_size_of _=(#size SDL_GPUColorTargetInfo)

sdl_gpu_color_target_info_alignment::Num a=>SDL_GPUColorTargetInfo->a
sdl_gpu_color_target_info_alignment _=(#alignment SDL_GPUColorTargetInfo)

sdl_gpu_color_target_info_peek::Ptr SDL_GPUColorTargetInfo->IO SDL_GPUColorTargetInfo
sdl_gpu_color_target_info_peek _=quick_error "sdl_gpu_color_target_info_peek" 0

sdl_gpu_color_target_info_poke::Ptr SDL_GPUColorTargetInfo->SDL_GPUColorTargetInfo->IO ()
sdl_gpu_color_target_info_poke ptr color_target_info=case color_target_info of
    SDL_GPUColorTargetInfo {sdl_texture,sdl_clear_color,sdl_load_op,sdl_store_op}->do
        fillBytes ptr 0 (#size SDL_GPUColorTargetInfo)
        (#poke SDL_GPUColorTargetInfo,texture) ptr sdl_texture
        (#poke SDL_GPUColorTargetInfo,clear_color) ptr sdl_clear_color
        (#poke SDL_GPUColorTargetInfo,load_op) ptr sdl_load_op
        (#poke SDL_GPUColorTargetInfo,store_op) ptr sdl_store_op

data SDL_GPUVertexAttribute=SDL_GPUVertexAttribute {sdl_location::Word32,sdl_buffer_slot::Word32,sdl_format::Word32,sdl_offset::Word32}

instance Storable SDL_GPUVertexAttribute where
    sizeOf=sdl_gpu_vertex_attribute_size_of
    alignment=sdl_gpu_vertex_attribute_alignment
    peek=sdl_gpu_vertex_attribute_peek
    poke=sdl_gpu_vertex_attribute_poke

sdl_gpu_vertex_attribute_size_of::Num a=>SDL_GPUVertexAttribute->a
sdl_gpu_vertex_attribute_size_of _=(#size SDL_GPUVertexAttribute)

sdl_gpu_vertex_attribute_alignment::Num a=>SDL_GPUVertexAttribute->a
sdl_gpu_vertex_attribute_alignment _=(#alignment SDL_GPUVertexAttribute)

sdl_gpu_vertex_attribute_peek::Ptr SDL_GPUVertexAttribute->IO SDL_GPUVertexAttribute
sdl_gpu_vertex_attribute_peek _=quick_error "sdl_gpu_vertex_attribute_peek" 0

sdl_gpu_vertex_attribute_poke::Ptr SDL_GPUVertexAttribute->SDL_GPUVertexAttribute->IO ()
sdl_gpu_vertex_attribute_poke ptr vertex_attribute=case vertex_attribute of
    SDL_GPUVertexAttribute {sdl_location,sdl_buffer_slot,sdl_format,sdl_offset}->do
        fillBytes ptr 0 (#size SDL_GPUVertexAttribute)
        (#poke SDL_GPUVertexAttribute,location) ptr sdl_location
        (#poke SDL_GPUVertexAttribute,buffer_slot) ptr sdl_buffer_slot
        (#poke SDL_GPUVertexAttribute,format) ptr sdl_format
        (#poke SDL_GPUVertexAttribute,offset) ptr sdl_offset

data SDL_GPUColorTargetDescription=SDL_GPUColorTargetDescription {sdl_format::Word32,sdl_blend_state::SDL_GPUColorTargetBlendState}

instance Storable SDL_GPUColorTargetDescription where
    sizeOf=sdl_gpu_color_target_description_size_of
    alignment=sdl_gpu_color_target_description_alignment
    peek=sdl_gpu_color_target_description_peek
    poke=sdl_gpu_color_target_description_poke

sdl_gpu_color_target_description_size_of::Num a=>SDL_GPUColorTargetDescription->a
sdl_gpu_color_target_description_size_of _=(#size SDL_GPUColorTargetDescription)

sdl_gpu_color_target_description_alignment::Num a=>SDL_GPUColorTargetDescription->a
sdl_gpu_color_target_description_alignment _=(#alignment SDL_GPUColorTargetDescription)

sdl_gpu_color_target_description_peek::Ptr SDL_GPUColorTargetDescription->IO SDL_GPUColorTargetDescription
sdl_gpu_color_target_description_peek _=quick_error "sdl_gpu_color_target_description_peek" 0

sdl_gpu_color_target_description_poke::Ptr SDL_GPUColorTargetDescription->SDL_GPUColorTargetDescription->IO ()
sdl_gpu_color_target_description_poke ptr color_target_description=case color_target_description of
    SDL_GPUColorTargetDescription {sdl_format,sdl_blend_state}->do
        fillBytes ptr 0 (#size SDL_GPUColorTargetDescription)
        (#poke SDL_GPUColorTargetDescription,format) ptr sdl_format
        (#poke SDL_GPUColorTargetDescription,blend_state) ptr sdl_blend_state

data SDL_GPUColorTargetBlendState=SDL_GPUColorTargetBlendState {sdl_src_color_blendfactor::Word32,sdl_dst_color_blendfactor::Word32,sdl_color_blend_op::Word32,sdl_src_alpha_blendfactor::Word32,sdl_dst_alpha_blendfactor::Word32,sdl_alpha_blend_op::Word32,sdl_color_write_mask::Word8,sdl_enable_blend::CBool,sdl_enable_color_write_mask::CBool}

instance Storable SDL_GPUColorTargetBlendState where
    sizeOf=sdl_gpu_color_target_blend_state_size_of
    alignment=sdl_gpu_color_target_blend_state_alignment
    peek=sdl_gpu_color_target_blend_state_peek
    poke=sdl_gpu_color_target_blend_state_poke

sdl_gpu_color_target_blend_state_size_of::Num a=>SDL_GPUColorTargetBlendState->a
sdl_gpu_color_target_blend_state_size_of _=(#size SDL_GPUColorTargetBlendState)

sdl_gpu_color_target_blend_state_alignment::Num a=>SDL_GPUColorTargetBlendState->a
sdl_gpu_color_target_blend_state_alignment _=(#alignment SDL_GPUColorTargetBlendState)

sdl_gpu_color_target_blend_state_peek::Ptr SDL_GPUColorTargetBlendState->IO SDL_GPUColorTargetBlendState
sdl_gpu_color_target_blend_state_peek _=quick_error "sdl_gpu_color_target_blend_state_peek" 0

sdl_gpu_color_target_blend_state_poke::Ptr SDL_GPUColorTargetBlendState->SDL_GPUColorTargetBlendState->IO ()
sdl_gpu_color_target_blend_state_poke ptr color_target_blend_state=case color_target_blend_state of
    SDL_GPUColorTargetBlendState {sdl_src_color_blendfactor,sdl_dst_color_blendfactor,sdl_color_blend_op,sdl_src_alpha_blendfactor,sdl_dst_alpha_blendfactor,sdl_alpha_blend_op,sdl_color_write_mask,sdl_enable_blend,sdl_enable_color_write_mask}->do
        fillBytes ptr 0 (#size SDL_GPUColorTargetBlendState)
        (#poke SDL_GPUColorTargetBlendState,src_color_blendfactor) ptr sdl_src_color_blendfactor
        (#poke SDL_GPUColorTargetBlendState,dst_color_blendfactor) ptr sdl_dst_color_blendfactor
        (#poke SDL_GPUColorTargetBlendState,color_blend_op) ptr sdl_color_blend_op
        (#poke SDL_GPUColorTargetBlendState,src_alpha_blendfactor) ptr sdl_src_alpha_blendfactor
        (#poke SDL_GPUColorTargetBlendState,dst_alpha_blendfactor) ptr sdl_dst_alpha_blendfactor
        (#poke SDL_GPUColorTargetBlendState,alpha_blend_op) ptr sdl_alpha_blend_op
        (#poke SDL_GPUColorTargetBlendState,color_write_mask) ptr sdl_color_write_mask
        (#poke SDL_GPUColorTargetBlendState,enable_blend) ptr sdl_enable_blend
        (#poke SDL_GPUColorTargetBlendState,enable_color_write_mask) ptr sdl_enable_color_write_mask

data SDL_GPUShaderCreateInfo=SDL_GPUShaderCreateInfo {sdl_code_size::CSize,sdl_code::Ptr Word8,sdl_entrypoint::CString,sdl_format::Word32,sdl_stage::Word32,sdl_num_samplers::Word32,sdl_num_storage_textures::Word32,sdl_num_storage_buffers::Word32,sdl_num_uniform_buffers::Word32}

instance Storable SDL_GPUShaderCreateInfo where
    sizeOf=sdl_gpu_shader_create_info_size_of
    alignment=sdl_gpu_shader_create_info_alignment
    peek=sdl_gpu_shader_create_info_peek
    poke=sdl_gpu_shader_create_info_poke

sdl_gpu_shader_create_info_size_of::Num a=>SDL_GPUShaderCreateInfo->a
sdl_gpu_shader_create_info_size_of _=(#size SDL_GPUShaderCreateInfo)

sdl_gpu_shader_create_info_alignment::Num a=>SDL_GPUShaderCreateInfo->a
sdl_gpu_shader_create_info_alignment _=(#alignment SDL_GPUShaderCreateInfo)

sdl_gpu_shader_create_info_peek::Ptr SDL_GPUShaderCreateInfo->IO SDL_GPUShaderCreateInfo
sdl_gpu_shader_create_info_peek _=quick_error "sdl_gpu_shader_create_info_peek" 0

sdl_gpu_shader_create_info_poke::Ptr SDL_GPUShaderCreateInfo->SDL_GPUShaderCreateInfo->IO ()
sdl_gpu_shader_create_info_poke ptr shader_create_info=case shader_create_info of
    SDL_GPUShaderCreateInfo {sdl_code_size,sdl_code,sdl_entrypoint,sdl_format,sdl_stage,sdl_num_samplers,sdl_num_storage_textures,sdl_num_storage_buffers,sdl_num_uniform_buffers}->do
        fillBytes ptr 0 (#size SDL_GPUShaderCreateInfo)
        (#poke SDL_GPUShaderCreateInfo,code_size) ptr sdl_code_size
        (#poke SDL_GPUShaderCreateInfo,code) ptr sdl_code
        (#poke SDL_GPUShaderCreateInfo,entrypoint) ptr sdl_entrypoint
        (#poke SDL_GPUShaderCreateInfo,format) ptr sdl_format
        (#poke SDL_GPUShaderCreateInfo,stage) ptr sdl_stage
        (#poke SDL_GPUShaderCreateInfo,num_samplers) ptr sdl_num_samplers
        (#poke SDL_GPUShaderCreateInfo,num_storage_textures) ptr sdl_num_storage_textures
        (#poke SDL_GPUShaderCreateInfo,num_storage_buffers) ptr sdl_num_storage_buffers
        (#poke SDL_GPUShaderCreateInfo,num_uniform_buffers) ptr sdl_num_uniform_buffers

data SDL_GPUVertexInputState=SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions::Ptr SDL_GPUVertexBufferDescription,sdl_num_vertex_buffers::Word32,sdl_vertex_attributes::Ptr SDL_GPUVertexAttribute,sdl_num_vertex_attributes::Word32}

instance Storable SDL_GPUVertexInputState where
    sizeOf=sdl_gpu_vertex_input_state_size_of
    alignment=sdl_gpu_vertex_input_state_alignment
    peek=sdl_gpu_vertex_input_state_peek
    poke=sdl_gpu_vertex_input_state_poke

sdl_gpu_vertex_input_state_size_of::Num a=>SDL_GPUVertexInputState->a
sdl_gpu_vertex_input_state_size_of _=(#size SDL_GPUVertexInputState)

sdl_gpu_vertex_input_state_alignment::Num a=>SDL_GPUVertexInputState->a
sdl_gpu_vertex_input_state_alignment _=(#alignment SDL_GPUVertexInputState)

sdl_gpu_vertex_input_state_peek::Ptr SDL_GPUVertexInputState->IO SDL_GPUVertexInputState
sdl_gpu_vertex_input_state_peek _=quick_error "sdl_gpu_vertex_input_state_peek" 0

sdl_gpu_vertex_input_state_poke::Ptr SDL_GPUVertexInputState->SDL_GPUVertexInputState->IO ()
sdl_gpu_vertex_input_state_poke ptr vertex_input_state=case vertex_input_state of
    SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions,sdl_num_vertex_buffers,sdl_vertex_attributes,sdl_num_vertex_attributes}->do
        fillBytes ptr 0 (#size SDL_GPUVertexInputState)
        (#poke SDL_GPUVertexInputState,vertex_buffer_descriptions) ptr sdl_vertex_buffer_descriptions
        (#poke SDL_GPUVertexInputState,num_vertex_buffers) ptr sdl_num_vertex_buffers
        (#poke SDL_GPUVertexInputState,vertex_attributes) ptr sdl_vertex_attributes
        (#poke SDL_GPUVertexInputState,num_vertex_attributes) ptr sdl_num_vertex_attributes

data SDL_GPUBufferCreateInfo=SDL_GPUBufferCreateInfo {sdl_usage::Word32,sdl_size::Word32}

instance Storable SDL_GPUBufferCreateInfo where
    sizeOf=sdl_gpu_buffer_create_info_size_of
    alignment=sdl_gpu_buffer_create_info_alignment
    peek=sdl_gpu_buffer_create_info_peek
    poke=sdl_gpu_buffer_create_info_poke

sdl_gpu_buffer_create_info_size_of::Num a=>SDL_GPUBufferCreateInfo->a
sdl_gpu_buffer_create_info_size_of _=(#size SDL_GPUBufferCreateInfo)

sdl_gpu_buffer_create_info_alignment::Num a=>SDL_GPUBufferCreateInfo->a
sdl_gpu_buffer_create_info_alignment _=(#alignment SDL_GPUBufferCreateInfo)

sdl_gpu_buffer_create_info_peek::Ptr SDL_GPUBufferCreateInfo->IO SDL_GPUBufferCreateInfo
sdl_gpu_buffer_create_info_peek _=quick_error "sdl_gpu_buffer_create_info_peek" 0

sdl_gpu_buffer_create_info_poke::Ptr SDL_GPUBufferCreateInfo->SDL_GPUBufferCreateInfo->IO ()
sdl_gpu_buffer_create_info_poke ptr buffer_create_info=case buffer_create_info of
    SDL_GPUBufferCreateInfo {sdl_usage,sdl_size}->do
        fillBytes ptr 0 (#size SDL_GPUBufferCreateInfo)
        (#poke SDL_GPUBufferCreateInfo,usage) ptr sdl_usage
        (#poke SDL_GPUBufferCreateInfo,size) ptr sdl_size

data SDL_GPUTransferBufferCreateInfo=SDL_GPUTransferBufferCreateInfo {sdl_usage::Word32,sdl_size::Word32}

instance Storable SDL_GPUTransferBufferCreateInfo where
    sizeOf=sdl_gpu_transfer_buffer_create_info_size_of
    alignment=sdl_gpu_transfer_buffer_create_info_alignment
    peek=sdl_gpu_transfer_buffer_create_info_peek
    poke=sdl_gpu_transfer_buffer_create_info_poke

sdl_gpu_transfer_buffer_create_info_size_of::Num a=>SDL_GPUTransferBufferCreateInfo->a
sdl_gpu_transfer_buffer_create_info_size_of _=(#size SDL_GPUTransferBufferCreateInfo)

sdl_gpu_transfer_buffer_create_info_alignment::Num a=>SDL_GPUTransferBufferCreateInfo->a
sdl_gpu_transfer_buffer_create_info_alignment _=(#alignment SDL_GPUTransferBufferCreateInfo)

sdl_gpu_transfer_buffer_create_info_peek::Ptr SDL_GPUTransferBufferCreateInfo->IO SDL_GPUTransferBufferCreateInfo
sdl_gpu_transfer_buffer_create_info_peek _=quick_error "sdl_gpu_transfer_buffer_create_info_peek" 0

sdl_gpu_transfer_buffer_create_info_poke::Ptr SDL_GPUTransferBufferCreateInfo->SDL_GPUTransferBufferCreateInfo->IO ()
sdl_gpu_transfer_buffer_create_info_poke ptr transfer_buffer_create_info=case transfer_buffer_create_info of
    SDL_GPUTransferBufferCreateInfo {sdl_usage,sdl_size}->do
        fillBytes ptr 0 (#size SDL_GPUTransferBufferCreateInfo)
        (#poke SDL_GPUTransferBufferCreateInfo,usage) ptr sdl_usage
        (#poke SDL_GPUTransferBufferCreateInfo,size) ptr sdl_size

data SDL_GPUTransferBufferLocation=SDL_GPUTransferBufferLocation {sdl_transfer_buffer::Ptr SDL_GPUTransferBuffer,sdl_offset::Word32}

instance Storable SDL_GPUTransferBufferLocation where
    sizeOf=sdl_gpu_transfer_buffer_location_size_of
    alignment=sdl_gpu_transfer_buffer_location_alignment
    peek=sdl_gpu_transfer_buffer_location_peek
    poke=sdl_gpu_transfer_buffer_location_poke

sdl_gpu_transfer_buffer_location_size_of::Num a=>SDL_GPUTransferBufferLocation->a
sdl_gpu_transfer_buffer_location_size_of _=(#size SDL_GPUTransferBufferLocation)

sdl_gpu_transfer_buffer_location_alignment::Num a=>SDL_GPUTransferBufferLocation->a
sdl_gpu_transfer_buffer_location_alignment _=(#alignment SDL_GPUTransferBufferLocation)

sdl_gpu_transfer_buffer_location_peek::Ptr SDL_GPUTransferBufferLocation->IO SDL_GPUTransferBufferLocation
sdl_gpu_transfer_buffer_location_peek _=quick_error "sdl_gpu_transfer_buffer_location_peek" 0

sdl_gpu_transfer_buffer_location_poke::Ptr SDL_GPUTransferBufferLocation->SDL_GPUTransferBufferLocation->IO ()
sdl_gpu_transfer_buffer_location_poke ptr transfer_buffer_location=case transfer_buffer_location of
    SDL_GPUTransferBufferLocation {sdl_transfer_buffer,sdl_offset}->do
        fillBytes ptr 0 (#size SDL_GPUTransferBufferLocation)
        (#poke SDL_GPUTransferBufferLocation,transfer_buffer) ptr sdl_transfer_buffer
        (#poke SDL_GPUTransferBufferLocation,offset) ptr sdl_offset

data SDL_GPUBufferRegion=SDL_GPUBufferRegion {sdl_buffer::Ptr SDL_GPUBuffer,sdl_offset::Word32,sdl_size::Word32}

instance Storable SDL_GPUBufferRegion where
    sizeOf=sdl_gpu_buffer_region_size_of
    alignment=sdl_gpu_buffer_region_alignment
    peek=sdl_gpu_buffer_region_peek
    poke=sdl_gpu_buffer_region_poke

sdl_gpu_buffer_region_size_of::Num a=>SDL_GPUBufferRegion->a
sdl_gpu_buffer_region_size_of _=(#size SDL_GPUBufferRegion)

sdl_gpu_buffer_region_alignment::Num a=>SDL_GPUBufferRegion->a
sdl_gpu_buffer_region_alignment _=(#alignment SDL_GPUBufferRegion)

sdl_gpu_buffer_region_peek::Ptr SDL_GPUBufferRegion->IO SDL_GPUBufferRegion
sdl_gpu_buffer_region_peek _=quick_error "sdl_gpu_buffer_region_peek" 0

sdl_gpu_buffer_region_poke::Ptr SDL_GPUBufferRegion->SDL_GPUBufferRegion->IO ()
sdl_gpu_buffer_region_poke ptr buffer_region=case buffer_region of
    SDL_GPUBufferRegion {sdl_buffer,sdl_offset,sdl_size}->do
        fillBytes ptr 0 (#size SDL_GPUBufferRegion)
        (#poke SDL_GPUBufferRegion,buffer) ptr sdl_buffer
        (#poke SDL_GPUBufferRegion,offset) ptr sdl_offset
        (#poke SDL_GPUBufferRegion,size) ptr sdl_size

data SDL_GPUVertexBufferDescription=SDL_GPUVertexBufferDescription {sdl_slot::Word32,sdl_pitch::Word32,sdl_input_rate::Word32,sdl_instance_step_rate::Word32}

instance Storable SDL_GPUVertexBufferDescription where
    sizeOf=sdl_gpu_vertex_buffer_description_size_of
    alignment=sdl_gpu_vertex_buffer_description_alignment
    peek=sdl_gpu_vertex_buffer_description_peek
    poke=sdl_gpu_vertex_buffer_description_poke

sdl_gpu_vertex_buffer_description_size_of::Num a=>SDL_GPUVertexBufferDescription->a
sdl_gpu_vertex_buffer_description_size_of _=(#size SDL_GPUVertexBufferDescription)

sdl_gpu_vertex_buffer_description_alignment::Num a=>SDL_GPUVertexBufferDescription->a
sdl_gpu_vertex_buffer_description_alignment _=(#alignment SDL_GPUVertexBufferDescription)

sdl_gpu_vertex_buffer_description_peek::Ptr SDL_GPUVertexBufferDescription->IO SDL_GPUVertexBufferDescription
sdl_gpu_vertex_buffer_description_peek _=quick_error "sdl_gpu_vertex_buffer_description_peek" 0

sdl_gpu_vertex_buffer_description_poke::Ptr SDL_GPUVertexBufferDescription->SDL_GPUVertexBufferDescription->IO ()
sdl_gpu_vertex_buffer_description_poke ptr vertex_buffer_description=case vertex_buffer_description of
    SDL_GPUVertexBufferDescription {sdl_slot,sdl_pitch,sdl_input_rate,sdl_instance_step_rate}->do
        fillBytes ptr 0 (#size SDL_GPUVertexBufferDescription)
        (#poke SDL_GPUVertexBufferDescription,slot) ptr sdl_slot
        (#poke SDL_GPUVertexBufferDescription,pitch) ptr sdl_pitch
        (#poke SDL_GPUVertexBufferDescription,input_rate) ptr sdl_input_rate
        (#poke SDL_GPUVertexBufferDescription,instance_step_rate) ptr sdl_instance_step_rate

data SDL_GPUGraphicsPipelineTargetInfo=SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions::Ptr SDL_GPUColorTargetDescription,sdl_num_color_targets::Word32,sdl_has_depth_stencil_target::CBool}

instance Storable SDL_GPUGraphicsPipelineTargetInfo where
    sizeOf=sdl_gpu_graphics_pipeline_target_info_size_of
    alignment=sdl_gpu_graphics_pipeline_target_info_alignment
    peek=sdl_gpu_graphics_pipeline_target_info_peek
    poke=sdl_gpu_graphics_pipeline_target_info_poke

sdl_gpu_graphics_pipeline_target_info_size_of::Num a=>SDL_GPUGraphicsPipelineTargetInfo->a
sdl_gpu_graphics_pipeline_target_info_size_of _=(#size SDL_GPUGraphicsPipelineTargetInfo)

sdl_gpu_graphics_pipeline_target_info_alignment::Num a=>SDL_GPUGraphicsPipelineTargetInfo->a
sdl_gpu_graphics_pipeline_target_info_alignment _=(#alignment SDL_GPUGraphicsPipelineTargetInfo)

sdl_gpu_graphics_pipeline_target_info_peek::Ptr SDL_GPUGraphicsPipelineTargetInfo->IO SDL_GPUGraphicsPipelineTargetInfo
sdl_gpu_graphics_pipeline_target_info_peek _=quick_error "sdl_gpu_graphics_pipeline_target_info_peek" 0

sdl_gpu_graphics_pipeline_target_info_poke::Ptr SDL_GPUGraphicsPipelineTargetInfo->SDL_GPUGraphicsPipelineTargetInfo->IO ()
sdl_gpu_graphics_pipeline_target_info_poke ptr graphics_pipeline_target_info=case graphics_pipeline_target_info of
    SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions,sdl_num_color_targets,sdl_has_depth_stencil_target}->do
        fillBytes ptr 0 (#size SDL_GPUGraphicsPipelineTargetInfo)
        (#poke SDL_GPUGraphicsPipelineTargetInfo,color_target_descriptions) ptr sdl_color_target_descriptions
        (#poke SDL_GPUGraphicsPipelineTargetInfo,num_color_targets) ptr sdl_num_color_targets
        (#poke SDL_GPUGraphicsPipelineTargetInfo,has_depth_stencil_target) ptr sdl_has_depth_stencil_target

data SDL_GPUGraphicsPipelineCreateInfo=SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader::Ptr SDL_GPUShader,sdl_fragment_shader::Ptr SDL_GPUShader,sdl_vertex_input_state::SDL_GPUVertexInputState,sdl_primitive_type::Word32,sdl_target_info::SDL_GPUGraphicsPipelineTargetInfo}

instance Storable SDL_GPUGraphicsPipelineCreateInfo where
    sizeOf=sdl_gpu_graphics_pipeline_create_info_size_of
    alignment=sdl_gpu_graphics_pipeline_create_info_alignment
    peek=sdl_gpu_graphics_pipeline_create_info_peek
    poke=sdl_gpu_graphics_pipeline_create_info_poke

sdl_gpu_graphics_pipeline_create_info_size_of::Num a=>SDL_GPUGraphicsPipelineCreateInfo->a
sdl_gpu_graphics_pipeline_create_info_size_of _=(#size SDL_GPUGraphicsPipelineCreateInfo)

sdl_gpu_graphics_pipeline_create_info_alignment::Num a=>SDL_GPUGraphicsPipelineCreateInfo->a
sdl_gpu_graphics_pipeline_create_info_alignment _=(#alignment SDL_GPUGraphicsPipelineCreateInfo)

sdl_gpu_graphics_pipeline_create_info_peek::Ptr SDL_GPUGraphicsPipelineCreateInfo->IO SDL_GPUGraphicsPipelineCreateInfo
sdl_gpu_graphics_pipeline_create_info_peek _=quick_error "sdl_gpu_graphics_pipeline_create_info_peek" 0

sdl_gpu_graphics_pipeline_create_info_poke::Ptr SDL_GPUGraphicsPipelineCreateInfo->SDL_GPUGraphicsPipelineCreateInfo->IO ()
sdl_gpu_graphics_pipeline_create_info_poke ptr graphics_pipeline_create_info=case graphics_pipeline_create_info of
    SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader,sdl_fragment_shader,sdl_vertex_input_state,sdl_primitive_type,sdl_target_info}->do
        fillBytes ptr 0 (#size SDL_GPUGraphicsPipelineCreateInfo)
        (#poke SDL_GPUGraphicsPipelineCreateInfo,vertex_shader) ptr sdl_vertex_shader
        (#poke SDL_GPUGraphicsPipelineCreateInfo,fragment_shader) ptr sdl_fragment_shader
        (#poke SDL_GPUGraphicsPipelineCreateInfo,vertex_input_state) ptr sdl_vertex_input_state
        (#poke SDL_GPUGraphicsPipelineCreateInfo,primitive_type) ptr sdl_primitive_type
        (#poke SDL_GPUGraphicsPipelineCreateInfo,target_info) ptr sdl_target_info

data SDL_GPUTextureCreateInfo=SDL_GPUTextureCreateInfo {sdl_type::Word32,sdl_format::Word32,sdl_usage::Word32,sdl_width::Word32,sdl_height::Word32,sdl_layer_count_or_depth::Word32,sdl_num_levels::Word32,sdl_sample_count::Word32}

instance Storable SDL_GPUTextureCreateInfo where
    sizeOf=sdl_gpu_texture_create_info_size_of
    alignment=sdl_gpu_texture_create_info_alignment
    peek=sdl_gpu_texture_create_info_peek
    poke=sdl_gpu_texture_create_info_poke

sdl_gpu_texture_create_info_size_of::Num a=>SDL_GPUTextureCreateInfo->a
sdl_gpu_texture_create_info_size_of _=(#size SDL_GPUTextureCreateInfo)

sdl_gpu_texture_create_info_alignment::Num a=>SDL_GPUTextureCreateInfo->a
sdl_gpu_texture_create_info_alignment _=(#alignment SDL_GPUTextureCreateInfo)

sdl_gpu_texture_create_info_peek::Ptr SDL_GPUTextureCreateInfo->IO SDL_GPUTextureCreateInfo
sdl_gpu_texture_create_info_peek _=quick_error "sdl_gpu_texture_create_info_peek" 0

sdl_gpu_texture_create_info_poke::Ptr SDL_GPUTextureCreateInfo->SDL_GPUTextureCreateInfo->IO ()
sdl_gpu_texture_create_info_poke ptr texture_create_info=case texture_create_info of
    SDL_GPUTextureCreateInfo {sdl_type,sdl_format,sdl_usage,sdl_width,sdl_height,sdl_layer_count_or_depth,sdl_num_levels,sdl_sample_count}->do
        fillBytes ptr 0 (#size SDL_GPUTextureCreateInfo)
        (#poke SDL_GPUTextureCreateInfo,type) ptr sdl_type
        (#poke SDL_GPUTextureCreateInfo,format) ptr sdl_format
        (#poke SDL_GPUTextureCreateInfo,usage) ptr sdl_usage
        (#poke SDL_GPUTextureCreateInfo,width) ptr sdl_width
        (#poke SDL_GPUTextureCreateInfo,height) ptr sdl_height
        (#poke SDL_GPUTextureCreateInfo,layer_count_or_depth) ptr sdl_layer_count_or_depth
        (#poke SDL_GPUTextureCreateInfo,num_levels) ptr sdl_num_levels
        (#poke SDL_GPUTextureCreateInfo,sample_count) ptr sdl_sample_count

data SDL_GPUSamplerCreateInfo=SDL_GPUSamplerCreateInfo {sdl_min_filter::Word32,sdl_mag_filter::Word32,sdl_mipmap_mode::Word32,sdl_address_mode_u::Word32,sdl_address_mode_v::Word32,sdl_address_mode_w::Word32}

instance Storable SDL_GPUSamplerCreateInfo where
    sizeOf=sdl_gpu_sampler_create_info_size_of
    alignment=sdl_gpu_sampler_create_info_alignment
    peek=sdl_gpu_sampler_create_info_peek
    poke=sdl_gpu_sampler_create_info_poke

sdl_gpu_sampler_create_info_size_of::Num a=>SDL_GPUSamplerCreateInfo->a
sdl_gpu_sampler_create_info_size_of _=(#size SDL_GPUSamplerCreateInfo)

sdl_gpu_sampler_create_info_alignment::Num a=>SDL_GPUSamplerCreateInfo->a
sdl_gpu_sampler_create_info_alignment _=(#alignment SDL_GPUSamplerCreateInfo)

sdl_gpu_sampler_create_info_peek::Ptr SDL_GPUSamplerCreateInfo->IO SDL_GPUSamplerCreateInfo
sdl_gpu_sampler_create_info_peek _=quick_error "sdl_gpu_sampler_create_info_peek" 0

sdl_gpu_sampler_create_info_poke::Ptr SDL_GPUSamplerCreateInfo->SDL_GPUSamplerCreateInfo->IO ()
sdl_gpu_sampler_create_info_poke ptr sampler_create_info=case sampler_create_info of
    SDL_GPUSamplerCreateInfo {sdl_min_filter,sdl_mag_filter,sdl_mipmap_mode,sdl_address_mode_u,sdl_address_mode_v,sdl_address_mode_w}->do
        fillBytes ptr 0 (#size SDL_GPUSamplerCreateInfo)
        (#poke SDL_GPUSamplerCreateInfo,min_filter) ptr sdl_min_filter
        (#poke SDL_GPUSamplerCreateInfo,mag_filter) ptr sdl_mag_filter
        (#poke SDL_GPUSamplerCreateInfo,mipmap_mode) ptr sdl_mipmap_mode
        (#poke SDL_GPUSamplerCreateInfo,address_mode_u) ptr sdl_address_mode_u
        (#poke SDL_GPUSamplerCreateInfo,address_mode_v) ptr sdl_address_mode_v
        (#poke SDL_GPUSamplerCreateInfo,address_mode_w) ptr sdl_address_mode_w

data SDL_GPUTextureSamplerBinding=SDL_GPUTextureSamplerBinding {sdl_texture::Ptr SDL_GPUTexture,sdl_sampler::Ptr SDL_GPUSampler}

instance Storable SDL_GPUTextureSamplerBinding where
    sizeOf=sdl_gpu_texture_sampler_binding_size_of
    alignment=sdl_gpu_texture_sampler_binding_alignment
    peek=sdl_gpu_texture_sampler_binding_peek
    poke=sdl_gpu_texture_sampler_binding_poke

sdl_gpu_texture_sampler_binding_size_of::Num a=>SDL_GPUTextureSamplerBinding->a
sdl_gpu_texture_sampler_binding_size_of _=(#size SDL_GPUTextureSamplerBinding)

sdl_gpu_texture_sampler_binding_alignment::Num a=>SDL_GPUTextureSamplerBinding->a
sdl_gpu_texture_sampler_binding_alignment _=(#alignment SDL_GPUTextureSamplerBinding)

sdl_gpu_texture_sampler_binding_peek::Ptr SDL_GPUTextureSamplerBinding->IO SDL_GPUTextureSamplerBinding
sdl_gpu_texture_sampler_binding_peek _=quick_error "sdl_gpu_texture_sampler_binding_peek" 0

sdl_gpu_texture_sampler_binding_poke::Ptr SDL_GPUTextureSamplerBinding->SDL_GPUTextureSamplerBinding->IO ()
sdl_gpu_texture_sampler_binding_poke ptr texture_sampler_binding=case texture_sampler_binding of
    SDL_GPUTextureSamplerBinding {sdl_texture,sdl_sampler}->do
        fillBytes ptr 0 (#size SDL_GPUTextureSamplerBinding)
        (#poke SDL_GPUTextureSamplerBinding,texture) ptr sdl_texture
        (#poke SDL_GPUTextureSamplerBinding,sampler) ptr sdl_sampler

data SDL_GPUTextureTransferInfo=SDL_GPUTextureTransferInfo {sdl_transfer_buffer::Ptr SDL_GPUTransferBuffer,sdl_offset::Word32,sdl_pixels_per_row::Word32,sdl_rows_per_layer::Word32}

instance Storable SDL_GPUTextureTransferInfo where
    sizeOf=sdl_gpu_texture_transfer_info_size_of
    alignment=sdl_gpu_texture_transfer_info_alignment
    peek=sdl_gpu_texture_transfer_info_peek
    poke=sdl_gpu_texture_transfer_info_poke

sdl_gpu_texture_transfer_info_size_of::Num a=>SDL_GPUTextureTransferInfo->a
sdl_gpu_texture_transfer_info_size_of _=(#size SDL_GPUTextureTransferInfo)

sdl_gpu_texture_transfer_info_alignment::Num a=>SDL_GPUTextureTransferInfo->a
sdl_gpu_texture_transfer_info_alignment _=(#alignment SDL_GPUTextureTransferInfo)

sdl_gpu_texture_transfer_info_peek::Ptr SDL_GPUTextureTransferInfo->IO SDL_GPUTextureTransferInfo
sdl_gpu_texture_transfer_info_peek _=quick_error "sdl_gpu_texture_transfer_info_peek" 0

sdl_gpu_texture_transfer_info_poke::Ptr SDL_GPUTextureTransferInfo->SDL_GPUTextureTransferInfo->IO ()
sdl_gpu_texture_transfer_info_poke ptr texture_transfer_info=case texture_transfer_info of
    SDL_GPUTextureTransferInfo {sdl_transfer_buffer,sdl_offset,sdl_pixels_per_row,sdl_rows_per_layer}->do
        fillBytes ptr 0 (#size SDL_GPUTextureTransferInfo)
        (#poke SDL_GPUTextureTransferInfo,transfer_buffer) ptr sdl_transfer_buffer
        (#poke SDL_GPUTextureTransferInfo,offset) ptr sdl_offset
        (#poke SDL_GPUTextureTransferInfo,pixels_per_row) ptr sdl_pixels_per_row
        (#poke SDL_GPUTextureTransferInfo,rows_per_layer) ptr sdl_rows_per_layer

data SDL_GPUTextureRegion=SDL_GPUTextureRegion {sdl_texture::Ptr SDL_GPUTexture,sdl_mip_level::Word32,sdl_layer::Word32,sdl_x::Word32,sdl_y::Word32,sdl_z::Word32,sdl_w::Word32,sdl_h::Word32,sdl_d::Word32}

instance Storable SDL_GPUTextureRegion where
    sizeOf=sdl_gpu_texture_region_size_of
    alignment=sdl_gpu_texture_region_alignment
    peek=sdl_gpu_texture_region_peek
    poke=sdl_gpu_texture_region_poke

sdl_gpu_texture_region_size_of::Num a=>SDL_GPUTextureRegion->a
sdl_gpu_texture_region_size_of _=(#size SDL_GPUTextureRegion)

sdl_gpu_texture_region_alignment::Num a=>SDL_GPUTextureRegion->a
sdl_gpu_texture_region_alignment _=(#alignment SDL_GPUTextureRegion)

sdl_gpu_texture_region_peek::Ptr SDL_GPUTextureRegion->IO SDL_GPUTextureRegion
sdl_gpu_texture_region_peek _=quick_error "sdl_gpu_texture_region_peek" 0

sdl_gpu_texture_region_poke::Ptr SDL_GPUTextureRegion->SDL_GPUTextureRegion->IO ()
sdl_gpu_texture_region_poke ptr texture_region=case texture_region of
    SDL_GPUTextureRegion {sdl_texture,sdl_mip_level,sdl_layer,sdl_x,sdl_y,sdl_z,sdl_w,sdl_h,sdl_d}->do
        fillBytes ptr 0 (#size SDL_GPUTextureRegion)
        (#poke SDL_GPUTextureRegion,texture) ptr sdl_texture
        (#poke SDL_GPUTextureRegion,mip_level) ptr sdl_mip_level
        (#poke SDL_GPUTextureRegion,layer) ptr sdl_layer
        (#poke SDL_GPUTextureRegion,x) ptr sdl_x
        (#poke SDL_GPUTextureRegion,y) ptr sdl_y
        (#poke SDL_GPUTextureRegion,z) ptr sdl_z
        (#poke SDL_GPUTextureRegion,w) ptr sdl_w
        (#poke SDL_GPUTextureRegion,h) ptr sdl_h
        (#poke SDL_GPUTextureRegion,d) ptr sdl_d

data SDL_GPUTextureLocation=SDL_GPUTextureLocation {sdl_texture::Ptr SDL_GPUTexture,sdl_mip_level::Word32,sdl_layer::Word32,sdl_x::Word32,sdl_y::Word32,sdl_z::Word32}

instance Storable SDL_GPUTextureLocation where
    sizeOf=sdl_gpu_texture_location_size_of
    alignment=sdl_gpu_texture_location_alignment
    peek=sdl_gpu_texture_location_peek
    poke=sdl_gpu_texture_location_poke

sdl_gpu_texture_location_size_of::Num a=>SDL_GPUTextureLocation->a
sdl_gpu_texture_location_size_of _=(#size SDL_GPUTextureLocation)

sdl_gpu_texture_location_alignment::Num a=>SDL_GPUTextureLocation->a
sdl_gpu_texture_location_alignment _=(#alignment SDL_GPUTextureLocation)

sdl_gpu_texture_location_peek::Ptr SDL_GPUTextureLocation->IO SDL_GPUTextureLocation
sdl_gpu_texture_location_peek _=quick_error "sdl_gpu_texture_location_peek" 0

sdl_gpu_texture_location_poke::Ptr SDL_GPUTextureLocation->SDL_GPUTextureLocation->IO ()
sdl_gpu_texture_location_poke ptr texture_location=case texture_location of
    SDL_GPUTextureLocation {sdl_texture,sdl_mip_level,sdl_layer,sdl_x,sdl_y,sdl_z}->do
        fillBytes ptr 0 (#size SDL_GPUTextureLocation)
        (#poke SDL_GPUTextureLocation,texture) ptr sdl_texture
        (#poke SDL_GPUTextureLocation,mip_level) ptr sdl_mip_level
        (#poke SDL_GPUTextureLocation,layer) ptr sdl_layer
        (#poke SDL_GPUTextureLocation,x) ptr sdl_x
        (#poke SDL_GPUTextureLocation,y) ptr sdl_y
        (#poke SDL_GPUTextureLocation,z) ptr sdl_z

data IMG_Animation=IMG_Animation {img_w::CInt,img_h::CInt,img_count::CInt,img_frames::Ptr (Ptr SDL_Surface),img_delays::Ptr CInt}

instance Storable IMG_Animation where
    sizeOf=img_animation_size_of
    alignment=img_animation_alignment
    peek=img_animation_peek
    poke=img_animation_poke

img_animation_size_of::Num a=>IMG_Animation->a
img_animation_size_of _=(#size IMG_Animation)

img_animation_alignment::Num a=>IMG_Animation->a
img_animation_alignment _=(#alignment IMG_Animation)

img_animation_peek::Ptr IMG_Animation->IO IMG_Animation
img_animation_peek ptr=do
    img_w<-(#peek IMG_Animation,w) ptr
    img_h<-(#peek IMG_Animation,h) ptr
    img_count<-(#peek IMG_Animation,count) ptr
    img_frames<-(#peek IMG_Animation,frames) ptr
    img_delays<-(#peek IMG_Animation,delays) ptr
    return (IMG_Animation {img_w=img_w,img_h=img_h,img_count=img_count,img_frames=img_frames,img_delays=img_delays})

img_animation_poke::Ptr IMG_Animation->IMG_Animation->IO ()
img_animation_poke ptr animation=case animation of
    IMG_Animation {img_w,img_h,img_count,img_frames,img_delays}->do
        fillBytes ptr 0 (#size IMG_Animation)
        (#poke IMG_Animation,w) ptr img_w
        (#poke IMG_Animation,h) ptr img_h
        (#poke IMG_Animation,count) ptr img_count
        (#poke IMG_Animation,frames) ptr img_frames
        (#poke IMG_Animation,delays) ptr img_delays

sdl_init_video::Word32
sdl_init_video=(#const SDL_INIT_VIDEO)

sdl_gpu_swapchaincomposition_sdr::Word32
sdl_gpu_swapchaincomposition_sdr=(#const SDL_GPU_SWAPCHAINCOMPOSITION_SDR)

sdl_gpu_presentmode_mailbox::Word32
sdl_gpu_presentmode_mailbox=(#const SDL_GPU_PRESENTMODE_MAILBOX)

sdl_gpu_shaderformat_dxil::Word32
sdl_gpu_shaderformat_dxil=(#const SDL_GPU_SHADERFORMAT_DXIL)

sdl_gpu_loadop_clear::Word32
sdl_gpu_loadop_clear=(#const SDL_GPU_LOADOP_CLEAR)

sdl_gpu_storeop_store::Word32
sdl_gpu_storeop_store=(#const SDL_GPU_STOREOP_STORE)

sdl_gpu_transferbufferusage_upload::Word32
sdl_gpu_transferbufferusage_upload=(#const SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD)

sdl_gpu_vertexinputrate_vertex::Word32
sdl_gpu_vertexinputrate_vertex=(#const SDL_GPU_VERTEXINPUTRATE_VERTEX)

sdl_gpu_primitivetype_trianglelist::Word32
sdl_gpu_primitivetype_trianglelist=(#const SDL_GPU_PRIMITIVETYPE_TRIANGLELIST)

sdl_gpu_shaderstage_vertex::Word32
sdl_gpu_shaderstage_vertex=(#const SDL_GPU_SHADERSTAGE_VERTEX)

sdl_gpu_shaderstage_fragment::Word32
sdl_gpu_shaderstage_fragment=(#const SDL_GPU_SHADERSTAGE_FRAGMENT)

sdl_gpu_bufferusage_vertex::Word32
sdl_gpu_bufferusage_vertex=(#const SDL_GPU_BUFFERUSAGE_VERTEX)

sdl_gpu_bufferusage_index::Word32
sdl_gpu_bufferusage_index=(#const SDL_GPU_BUFFERUSAGE_INDEX)

sdl_gpu_bufferusage_graphics_storage_read::Word32
sdl_gpu_bufferusage_graphics_storage_read=(#const SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ)

sdl_gpu_vertexelementformat_float::Word32
sdl_gpu_vertexelementformat_float=(#const SDL_GPU_VERTEXELEMENTFORMAT_FLOAT)

sdl_gpu_vertexelementformat_float2::Word32
sdl_gpu_vertexelementformat_float2=(#const SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2)

sdl_gpu_vertexelementformat_float4::Word32
sdl_gpu_vertexelementformat_float4=(#const SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4)

sdl_gpu_blendfactor_src_alpha::Word32
sdl_gpu_blendfactor_src_alpha=(#const SDL_GPU_BLENDFACTOR_SRC_ALPHA)

sdl_gpu_blendfactor_one_minus_src_alpha::Word32
sdl_gpu_blendfactor_one_minus_src_alpha=(#const SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA)

sdl_gpu_blendfactor_one::Word32
sdl_gpu_blendfactor_one=(#const SDL_GPU_BLENDFACTOR_ONE)

sdl_gpu_blendfactor_zero::Word32
sdl_gpu_blendfactor_zero=(#const SDL_GPU_BLENDFACTOR_ZERO)

sdl_gpu_blendop_add::Word32
sdl_gpu_blendop_add=(#const SDL_GPU_BLENDOP_ADD)

sdl_gpu_colorcomponent_r::Word8
sdl_gpu_colorcomponent_r=(#const SDL_GPU_COLORCOMPONENT_R)

sdl_gpu_colorcomponent_g::Word8
sdl_gpu_colorcomponent_g=(#const SDL_GPU_COLORCOMPONENT_G)

sdl_gpu_colorcomponent_b::Word8
sdl_gpu_colorcomponent_b=(#const SDL_GPU_COLORCOMPONENT_B)

sdl_gpu_colorcomponent_a::Word8
sdl_gpu_colorcomponent_a=(#const SDL_GPU_COLORCOMPONENT_A)

sdl_gpu_indexelementsize_32bit::Word32
sdl_gpu_indexelementsize_32bit=(#const SDL_GPU_INDEXELEMENTSIZE_32BIT)

sdl_gpu_textureformat_r8g8b8a8_unorm::Word32
sdl_gpu_textureformat_r8g8b8a8_unorm=(#const SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM)

sdl_gpu_texturetype_2d::Word32
sdl_gpu_texturetype_2d=(#const SDL_GPU_TEXTURETYPE_2D)

sdl_gpu_textureusage_color_target::Word32
sdl_gpu_textureusage_color_target=(#const SDL_GPU_TEXTUREUSAGE_COLOR_TARGET)

sdl_gpu_textureusage_sampler::Word32
sdl_gpu_textureusage_sampler=(#const SDL_GPU_TEXTUREUSAGE_SAMPLER)

sdl_gpu_samplecount_1::Word32
sdl_gpu_samplecount_1=(#const SDL_GPU_SAMPLECOUNT_1)

sdl_gpu_filter_nearest::Word32
sdl_gpu_filter_nearest=(#const SDL_GPU_FILTER_NEAREST)

sdl_gpu_samplermipmapmode_nearest::Word32
sdl_gpu_samplermipmapmode_nearest=(#const SDL_GPU_SAMPLERMIPMAPMODE_NEAREST)

sdl_gpu_sampleraddressmode_clamp_to_edge::Word32
sdl_gpu_sampleraddressmode_clamp_to_edge=(#const SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE)

sdl_window_fullscreen::Word64
sdl_window_fullscreen=(#const SDL_WINDOW_FULLSCREEN)

sdl_window_hidden::Word64
sdl_window_hidden=(#const SDL_WINDOW_HIDDEN)

sdl_window_borderless::Word64
sdl_window_borderless=(#const SDL_WINDOW_BORDERLESS)

sdl_window_resizable::Word64
sdl_window_resizable=(#const SDL_WINDOW_RESIZABLE)

sdl_window_always_on_top::Word64
sdl_window_always_on_top=(#const SDL_WINDOW_ALWAYS_ON_TOP)

sdl_pixelformat_rgba32::Word32
sdl_pixelformat_rgba32=(#const SDL_PIXELFORMAT_RGBA32)

sdl_event_size::Int
sdl_event_size=(#size SDL_Event)

sdl_event_alignment::Int
sdl_event_alignment=(#alignment SDL_Event)

sdl_user_event_data1_poke::Ptr ()->Ptr ()->IO ()
sdl_user_event_data1_poke ptr data1=(#poke SDL_UserEvent,data1) ptr data1

sdl_user_event_data1_peek::Ptr ()->IO (Ptr ())
sdl_user_event_data1_peek ptr=(#peek SDL_UserEvent,data1) ptr

sdl_event_type_peek::Ptr ()->IO Word32
sdl_event_type_peek ptr=(#peek SDL_Event,type) ptr

sdl_windowevent_windowid_peek::Ptr ()->IO Word32
sdl_windowevent_windowid_peek ptr=(#peek SDL_WindowEvent,windowID) ptr

sdl_windowevent_data1_peek::Ptr ()->IO Word32
sdl_windowevent_data1_peek ptr=(#peek SDL_WindowEvent,data1) ptr

sdl_windowevent_data2_peek::Ptr ()->IO Word32
sdl_windowevent_data2_peek ptr=(#peek SDL_WindowEvent,data2) ptr

sdl_keyboardevent_windowid_peek::Ptr ()->IO Word32
sdl_keyboardevent_windowid_peek ptr=(#peek SDL_KeyboardEvent,windowID) ptr

sdl_keyboardevent_key_peek::Ptr ()->IO Word32
sdl_keyboardevent_key_peek ptr=(#peek SDL_KeyboardEvent,key) ptr

sdl_surface_w_peek::Ptr SDL_Surface->IO CInt
sdl_surface_w_peek ptr=(#peek SDL_Surface,w) ptr

sdl_surface_h_peek::Ptr SDL_Surface->IO CInt
sdl_surface_h_peek ptr=(#peek SDL_Surface,h) ptr

sdl_surface_pitch_peek::Ptr SDL_Surface->IO CInt
sdl_surface_pitch_peek ptr=(#peek SDL_Surface,pitch) ptr

sdl_surface_pixels_peek::Ptr SDL_Surface->IO (Ptr ())
sdl_surface_pixels_peek ptr=(#peek SDL_Surface,pixels) ptr

pattern SDL_EVENT_QUIT::Word32
pattern SDL_EVENT_QUIT=(#const SDL_EVENT_QUIT)

pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED::Word32
pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED=(#const SDL_EVENT_WINDOW_CLOSE_REQUESTED)

pattern SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED::Word32
pattern SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED=(#const SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED)

pattern SDL_EVENT_KEY_UP::Word32
pattern SDL_EVENT_KEY_UP=(#const SDL_EVENT_KEY_UP)

pattern SDL_EVENT_KEY_DOWN::Word32
pattern SDL_EVENT_KEY_DOWN=(#const SDL_EVENT_KEY_DOWN)

pattern SDLK_A::Word32
pattern SDLK_A=(#const SDLK_A)

pattern SDLK_B::Word32
pattern SDLK_B=(#const SDLK_B)

pattern SDLK_C::Word32
pattern SDLK_C=(#const SDLK_C)

pattern SDLK_D::Word32
pattern SDLK_D=(#const SDLK_D)

pattern SDLK_E::Word32
pattern SDLK_E=(#const SDLK_E)

pattern SDLK_F::Word32
pattern SDLK_F=(#const SDLK_F)

pattern SDLK_G::Word32
pattern SDLK_G=(#const SDLK_G)

pattern SDLK_H::Word32
pattern SDLK_H=(#const SDLK_H)

pattern SDLK_I::Word32
pattern SDLK_I=(#const SDLK_I)

pattern SDLK_J::Word32
pattern SDLK_J=(#const SDLK_J)

pattern SDLK_K::Word32
pattern SDLK_K=(#const SDLK_K)

pattern SDLK_L::Word32
pattern SDLK_L=(#const SDLK_L)

pattern SDLK_M::Word32
pattern SDLK_M=(#const SDLK_M)

pattern SDLK_N::Word32
pattern SDLK_N=(#const SDLK_N)

pattern SDLK_O::Word32
pattern SDLK_O=(#const SDLK_O)

pattern SDLK_P::Word32
pattern SDLK_P=(#const SDLK_P)

pattern SDLK_Q::Word32
pattern SDLK_Q=(#const SDLK_Q)

pattern SDLK_R::Word32
pattern SDLK_R=(#const SDLK_R)

pattern SDLK_S::Word32
pattern SDLK_S=(#const SDLK_S)

pattern SDLK_T::Word32
pattern SDLK_T=(#const SDLK_T)

pattern SDLK_U::Word32
pattern SDLK_U=(#const SDLK_U)

pattern SDLK_V::Word32
pattern SDLK_V=(#const SDLK_V)

pattern SDLK_W::Word32
pattern SDLK_W=(#const SDLK_W)

pattern SDLK_X::Word32
pattern SDLK_X=(#const SDLK_X)

pattern SDLK_Y::Word32
pattern SDLK_Y=(#const SDLK_Y)

pattern SDLK_Z::Word32
pattern SDLK_Z=(#const SDLK_Z)