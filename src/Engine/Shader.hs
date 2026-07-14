{-# LANGUAGE DuplicateRecordFields #-}

module Engine.Shader where

import Engine.Helper
import Engine.Type
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

standard_color_write_mask::DW.Word8
standard_color_write_mask=SDLI.sdl_gpu_colorcomponent_r DB..|. SDLI.sdl_gpu_colorcomponent_g DB..|. SDLI.sdl_gpu_colorcomponent_b DB..|. SDLI.sdl_gpu_colorcomponent_a

standard_blend_state::SDLI.SDL_GPUColorTargetBlendState
standard_blend_state=SDLI.SDL_GPUColorTargetBlendState {sdl_src_color_blendfactor=SDLI.sdl_gpu_blendfactor_src_alpha,sdl_dst_color_blendfactor=SDLI.sdl_gpu_blendfactor_one_minus_src_alpha,sdl_color_blend_op=SDLI.sdl_gpu_blendop_add,sdl_src_alpha_blendfactor=SDLI.sdl_gpu_blendfactor_one,sdl_dst_alpha_blendfactor=SDLI.sdl_gpu_blendfactor_zero,sdl_alpha_blend_op=SDLI.sdl_gpu_blendop_add,sdl_color_write_mask=standard_color_write_mask,sdl_enable_blend=FMU.fromBool True,sdl_enable_color_write_mask=FMU.fromBool True}

create_graphics_pipeline::FP.Ptr SDLT.SDL_Window->FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUShader->FP.Ptr SDLT.SDL_GPUShader->IO (FP.Ptr SDLT.SDL_GPUGraphicsPipeline)
create_graphics_pipeline sdl_window device vertex_shader fragment_shader=FMA.allocaArray 5 $ \vertex_attribute->do
    FS.pokeElemOff vertex_attribute 0 SDLI.SDL_GPUVertexAttribute {sdl_location=0,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float4,sdl_offset=0}
    FS.pokeElemOff vertex_attribute 1 SDLI.SDL_GPUVertexAttribute {sdl_location=1,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float2,sdl_offset=16}
    FS.pokeElemOff vertex_attribute 2 SDLI.SDL_GPUVertexAttribute {sdl_location=2,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float2,sdl_offset=24}
    FS.pokeElemOff vertex_attribute 3 SDLI.SDL_GPUVertexAttribute {sdl_location=3,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float,sdl_offset=32}
    FS.pokeElemOff vertex_attribute 4 SDLI.SDL_GPUVertexAttribute {sdl_location=4,sdl_buffer_slot=0,sdl_format=SDLI.sdl_gpu_vertexelementformat_float,sdl_offset=36}
    FMU.with SDLI.SDL_GPUVertexBufferDescription {sdl_slot=0,sdl_pitch=40,sdl_input_rate=SDLI.sdl_gpu_vertexinputrate_vertex,sdl_instance_step_rate=0} $ \vertex_buffer_description->do
        format<-SDLF.sdl_get_gpu_swapchain_texture_format device sdl_window
        FMU.with SDLI.SDL_GPUColorTargetDescription {sdl_format=format,sdl_blend_state=standard_blend_state} (\color_target_description->FMU.with SDLI.SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader=vertex_shader,sdl_fragment_shader=fragment_shader,sdl_vertex_input_state=SDLI.SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions=vertex_buffer_description,sdl_num_vertex_buffers=1,sdl_vertex_attributes=vertex_attribute,sdl_num_vertex_attributes=5},sdl_primitive_type=SDLI.sdl_gpu_primitivetype_trianglelist,sdl_target_info=SDLI.SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions=color_target_description,sdl_num_color_targets=1,sdl_has_depth_stencil_target=FMU.fromBool False}} (return_catch_null . SDLF.sdl_create_gpu_graphics_pipeline device))

update_buffer::FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUBuffer->FP.Ptr SDLT.SDL_GPUBuffer->FP.Ptr SDLT.SDL_GPUBuffer->FP.Ptr SDLT.SDL_GPUTransferBuffer->Int->Int->Int->DS.Seq Vertex->DS.Seq DW.Word32->DS.Seq Parameter->IO Bool
update_buffer device command_buffer vertex_buffer index_buffer parameter_buffer transfer_buffer vertex_size index_size parameter_size vertex index parameter=let vertex_length=DS.length vertex in let index_length=DS.length index in let parameter_length=DS.length parameter in if vertex_length==0||index_length==0||parameter_length==0 then return False else let unit_vertex=FS.sizeOf (undefined::Vertex) in let unit_index=FS.sizeOf (undefined::DW.Word32) in let unit_parameter=FS.sizeOf (undefined::Parameter) in let new_vertex_size=vertex_length*unit_vertex in let new_index_size=index_length*unit_index in let new_parameter_size=parameter_length*unit_parameter in if vertex_size<new_vertex_size||index_size<new_index_size||parameter_size<new_parameter_size then error "update_buffer: error 1" else do
    map_transfer_buffer<-SDLF.sdl_map_gpu_transfer_buffer device transfer_buffer (FMU.fromBool True)
    catch_null map_transfer_buffer
    seq_poke_array unit_vertex vertex (FP.castPtr map_transfer_buffer)
    seq_poke_array unit_index index (FP.plusPtr map_transfer_buffer vertex_size)
    let size=vertex_size+index_size
    seq_poke_array unit_parameter parameter (FP.plusPtr map_transfer_buffer size)
    SDLF.sdl_unmap_gpu_transfer_buffer device transfer_buffer
    copy_pass<-SDLF.sdl_begin_gpu_copy_pass command_buffer
    catch_null copy_pass
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=0}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=vertex_buffer,sdl_offset=0,sdl_size=fromIntegral new_vertex_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=fromIntegral vertex_size}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=index_buffer,sdl_offset=0,sdl_size=fromIntegral new_index_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=fromIntegral size}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=parameter_buffer,sdl_offset=0,sdl_size=fromIntegral new_parameter_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    SDLF.sdl_end_gpu_copy_pass copy_pass
    return True

load_shader::FP.Ptr SDLT.SDL_GPUDevice->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->String->IO (FP.Ptr SDLT.SDL_GPUShader)
load_shader device format stage num_sampler num_storage_buffer num_uniform_buffer path=do
    shader_code<-DBS.readFile path
    DBS.useAsCStringLen shader_code (\(code,code_size)->FCS.withCString "main" (\entrypoint->FMU.with (SDLI.SDL_GPUShaderCreateInfo {sdl_code_size=fromIntegral code_size,sdl_code=FP.castPtr code,sdl_entrypoint=entrypoint,sdl_format=format,sdl_stage=stage,sdl_num_samplers=num_sampler,sdl_num_storage_textures=0,sdl_num_storage_buffers=num_storage_buffer,sdl_num_uniform_buffers=num_uniform_buffer}) (return_catch_null . SDLF.sdl_create_gpu_shader device)))