{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Shader where

import Engine.Container
import Engine.Type
import Engine.Underlying
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

standard_color_target_blend_state::SDLI.SDL_GPUColorTargetBlendState
standard_color_target_blend_state=SDLI.SDL_GPUColorTargetBlendState {sdl_src_color_blendfactor=SDLI.sdl_gpu_blendfactor_src_alpha,sdl_dst_color_blendfactor=SDLI.sdl_gpu_blendfactor_one_minus_src_alpha,sdl_color_blend_op=SDLI.sdl_gpu_blendop_add,sdl_src_alpha_blendfactor=SDLI.sdl_gpu_blendfactor_one,sdl_dst_alpha_blendfactor=SDLI.sdl_gpu_blendfactor_zero,sdl_alpha_blend_op=SDLI.sdl_gpu_blendop_add,sdl_color_write_mask=SDLI.sdl_gpu_colorcomponent_r DB..|. SDLI.sdl_gpu_colorcomponent_g DB..|. SDLI.sdl_gpu_colorcomponent_b DB..|. SDLI.sdl_gpu_colorcomponent_a,sdl_enable_blend=FMU.fromBool True,sdl_enable_color_write_mask=FMU.fromBool True}

create_graphics_pipeline::FP.Ptr SDLT.SDL_Window->FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUShader->FP.Ptr SDLT.SDL_GPUShader->IO (FP.Ptr SDLT.SDL_GPUGraphicsPipeline)
create_graphics_pipeline window device vertex_shader fragment_shader=FMA.allocaArray 5 $ \vertex_attribute->do
    FS.pokeElemOff vertex_attribute 0 SDLI.SDL_GPUVertexAttribute {sdl_location=0,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float4,sdl_offset=0}
    FS.pokeElemOff vertex_attribute 1 SDLI.SDL_GPUVertexAttribute {sdl_location=1,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float2,sdl_offset=16}
    FS.pokeElemOff vertex_attribute 2 SDLI.SDL_GPUVertexAttribute {sdl_location=2,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float2,sdl_offset=24}
    FS.pokeElemOff vertex_attribute 3 SDLI.SDL_GPUVertexAttribute {sdl_location=3,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float,sdl_offset=32}
    FS.pokeElemOff vertex_attribute 4 SDLI.SDL_GPUVertexAttribute {sdl_location=4,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float,sdl_offset=36}
    FMU.with SDLI.SDL_GPUVertexBufferDescription {sdl_slot=0,sdl_pitch=40,sdl_input_rate=SDLI.sdl_gpu_vertexinputrate_vertex,sdl_instance_step_rate=0} $ \vertex_buffer_description->do
        format<-SDLF.sdl_get_gpu_swapchain_texture_format device window
        FMU.with SDLI.SDL_GPUColorTargetDescription {sdl_format=format,sdl_blend_state=standard_color_target_blend_state} (\color_target_description->FMU.with SDLI.SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader=vertex_shader,sdl_fragment_shader=fragment_shader,sdl_vertex_input_state=SDLI.SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions=vertex_buffer_description,sdl_num_vertex_buffers=1,sdl_vertex_attributes=vertex_attribute,sdl_num_vertex_attributes=5},sdl_primitive_type=SDLI.sdl_gpu_primitivetype_trianglelist,sdl_target_info=SDLI.SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions=color_target_description,sdl_num_color_targets=1,sdl_has_depth_stencil_target=FMU.fromBool False}} (return_catch_null . SDLF.sdl_create_gpu_graphics_pipeline device))

create_canvas_graphics_pipeline::FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUShader->FP.Ptr SDLT.SDL_GPUShader->IO (FP.Ptr SDLT.SDL_GPUGraphicsPipeline)
create_canvas_graphics_pipeline device vertex_shader fragment_shader=FMA.allocaArray 5 $ \vertex_attribute->do
    FS.pokeElemOff vertex_attribute 0 SDLI.SDL_GPUVertexAttribute {sdl_location=0,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float4,sdl_offset=0}
    FS.pokeElemOff vertex_attribute 1 SDLI.SDL_GPUVertexAttribute {sdl_location=1,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float2,sdl_offset=16}
    FS.pokeElemOff vertex_attribute 2 SDLI.SDL_GPUVertexAttribute {sdl_location=2,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float2,sdl_offset=24}
    FS.pokeElemOff vertex_attribute 3 SDLI.SDL_GPUVertexAttribute {sdl_location=3,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float,sdl_offset=32}
    FS.pokeElemOff vertex_attribute 4 SDLI.SDL_GPUVertexAttribute {sdl_location=4,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float,sdl_offset=36}
    FMU.with SDLI.SDL_GPUVertexBufferDescription {sdl_slot=0,sdl_pitch=40,sdl_input_rate=SDLI.sdl_gpu_vertexinputrate_vertex,sdl_instance_step_rate=0} $ \vertex_buffer_description->FMU.with SDLI.SDL_GPUColorTargetDescription {sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_blend_state=standard_color_target_blend_state} (\color_target_description->FMU.with SDLI.SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader=vertex_shader,sdl_fragment_shader=fragment_shader,sdl_vertex_input_state=SDLI.SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions=vertex_buffer_description,sdl_num_vertex_buffers=1,sdl_vertex_attributes=vertex_attribute,sdl_num_vertex_attributes=5},sdl_primitive_type=SDLI.sdl_gpu_primitivetype_trianglelist,sdl_target_info=SDLI.SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions=color_target_description,sdl_num_color_targets=1,sdl_has_depth_stencil_target=FMU.fromBool False}} (return_catch_null . SDLF.sdl_create_gpu_graphics_pipeline device))

load_shader::FP.Ptr SDLT.SDL_GPUDevice->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->String->IO (FP.Ptr SDLT.SDL_GPUShader)
load_shader device format stage num_sampler num_storage_buffer num_uniform_buffer path=do
    shader_code<-DBS.readFile path
    DBS.useAsCStringLen shader_code (\(code,code_size)->FCS.withCString "main" (\entrypoint->FMU.with (SDLI.SDL_GPUShaderCreateInfo {sdl_code_size=fromIntegral code_size,sdl_code=FP.castPtr code,sdl_entrypoint=entrypoint,sdl_format=format,sdl_stage=stage,sdl_num_samplers=num_sampler,sdl_num_storage_textures=0,sdl_num_storage_buffers=num_storage_buffer,sdl_num_uniform_buffers=num_uniform_buffer}) (return_catch_null . SDLF.sdl_create_gpu_shader device)))

insert_pipeline_id::Int->Shader->Shader
insert_pipeline_id this_pipeline_id shader=case shader of
    Shader {sdl_shader,pipeline_id}->Shader {sdl_shader=sdl_shader,pipeline_id=intset_insert this_pipeline_id pipeline_id}

delete_pipeline_id::Int->Shader->Shader
delete_pipeline_id this_pipeline_id shader=case shader of
    Shader {sdl_shader,pipeline_id}->Shader {sdl_shader=sdl_shader,pipeline_id=intset_delete this_pipeline_id pipeline_id}