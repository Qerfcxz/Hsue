{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Engine.Shader where

import Engine.Other
import Engine.Type
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified SDL.Type as T
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
standard_color_write_mask=C.sdl_gpu_colorcomponent_r DB..|. C.sdl_gpu_colorcomponent_g DB..|. C.sdl_gpu_colorcomponent_b DB..|. C.sdl_gpu_colorcomponent_a

standard_blend_state::C.SDL_GPUColorTargetBlendState
standard_blend_state=C.SDL_GPUColorTargetBlendState {sdl_src_color_blendfactor=C.sdl_gpu_blendfactor_src_alpha,sdl_dst_color_blendfactor=C.sdl_gpu_blendfactor_one_minus_src_alpha,sdl_color_blend_op=C.sdl_gpu_blendop_add,sdl_src_alpha_blendfactor=C.sdl_gpu_blendfactor_one,sdl_dst_alpha_blendfactor=C.sdl_gpu_blendfactor_zero,sdl_alpha_blend_op=C.sdl_gpu_blendop_add,sdl_color_write_mask=standard_color_write_mask,sdl_enable_blend=FMU.fromBool True,sdl_enable_color_write_mask=FMU.fromBool True}

create_graphics_pipeline::FP.Ptr T.SDL_Window->FP.Ptr T.SDL_GPUDevice->FP.Ptr T.SDL_GPUShader->FP.Ptr T.SDL_GPUShader->IO (FP.Ptr T.SDL_GPUGraphicsPipeline)
create_graphics_pipeline sdl_window device vertex_shader fragment_shader=FMA.allocaArray 5 $ \vertex_attribute->do
    FS.pokeElemOff vertex_attribute 0 C.SDL_GPUVertexAttribute {sdl_location=0,sdl_buffer_slot=0,sdl_format=C.sdl_gpu_vertexelementformat_float4,sdl_offset=0}
    FS.pokeElemOff vertex_attribute 1 C.SDL_GPUVertexAttribute {sdl_location=1,sdl_buffer_slot=0,sdl_format=C.sdl_gpu_vertexelementformat_float2,sdl_offset=16}
    FS.pokeElemOff vertex_attribute 2 C.SDL_GPUVertexAttribute {sdl_location=2,sdl_buffer_slot=0,sdl_format=C.sdl_gpu_vertexelementformat_float2,sdl_offset=24}
    FS.pokeElemOff vertex_attribute 3 C.SDL_GPUVertexAttribute {sdl_location=3,sdl_buffer_slot=0,sdl_format=C.sdl_gpu_vertexelementformat_float,sdl_offset=32}
    FS.pokeElemOff vertex_attribute 4 C.SDL_GPUVertexAttribute {sdl_location=4,sdl_buffer_slot=0,sdl_format=C.sdl_gpu_vertexelementformat_float,sdl_offset=36}
    FMU.with C.SDL_GPUVertexBufferDescription {sdl_slot=0,sdl_pitch=40,sdl_input_rate=C.sdl_gpu_vertexinputrate_vertex,sdl_instance_step_rate=0} $ \vertex_buffer_description->do
        format<-F.sdl_getgpuswapchaintextureformat device sdl_window
        FMU.with C.SDL_GPUColorTargetDescription {sdl_format=format,sdl_blend_state=standard_blend_state} (\color_target_description->FMU.with C.SDL_GPUGraphicsPipelineCreateInfo {sdl_vertex_shader=vertex_shader,sdl_fragment_shader=fragment_shader,sdl_vertex_input_state=C.SDL_GPUVertexInputState {sdl_vertex_buffer_descriptions=vertex_buffer_description,sdl_num_vertex_buffers=1,sdl_vertex_attributes=vertex_attribute,sdl_num_vertex_attributes=5},sdl_primitive_type=C.sdl_gpu_primitivetype_trianglelist,sdl_target_info=C.SDL_GPUGraphicsPipelineTargetInfo {sdl_color_target_descriptions=color_target_description,sdl_num_color_targets=1,sdl_has_depth_stencil_target=FMU.fromBool False}} (return_catch_null . F.sdl_creategpugraphicspipeline device))

update_buffer::FP.Ptr T.SDL_GPUDevice->FP.Ptr T.SDL_GPUCommandBuffer->FP.Ptr T.SDL_GPUBuffer->FP.Ptr T.SDL_GPUBuffer->FP.Ptr T.SDL_GPUBuffer->FP.Ptr T.SDL_GPUTransferBuffer->Int->Int->Int->DS.Seq Vertex->DS.Seq DW.Word32->DS.Seq Parameter->IO (Maybe ())
update_buffer device command_buffer vertex_buffer index_buffer parameter_buffer transfer_buffer vertex_size index_size parameter_size vertex index parameter=let vertex_length=DS.length vertex in let index_length=DS.length index in let parameter_length=DS.length parameter in if vertex_length==0||index_length==0||parameter_length==0 then return Nothing else let single_vertex=FS.sizeOf (undefined::Vertex) in let single_index=FS.sizeOf (undefined::DW.Word32) in let single_parameter=FS.sizeOf (undefined::Parameter) in let new_vertex_size=vertex_length*single_vertex in let new_index_size=index_length*single_index in let new_parameter_size=parameter_length*single_parameter in if vertex_size<new_vertex_size||index_size<new_index_size||parameter_size<new_parameter_size then error "update_buffer: error 1" else do
    map_transfer_buffer<-F.sdl_mapgputransferbuffer device transfer_buffer (FMU.fromBool True)
    catch_null map_transfer_buffer
    seq_poke_array single_vertex vertex (FP.castPtr map_transfer_buffer)
    seq_poke_array single_index index (FP.plusPtr map_transfer_buffer vertex_size)
    let size=vertex_size+index_size
    seq_poke_array single_parameter parameter (FP.plusPtr map_transfer_buffer size)
    F.sdl_unmapgputransferbuffer device transfer_buffer
    copy_pass<-F.sdl_begingpucopypass command_buffer
    catch_null copy_pass
    FMU.with (C.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=0}) (\transfer_buffer_location->FMU.with (C.SDL_GPUBufferRegion {sdl_buffer=vertex_buffer,sdl_offset=0,sdl_size=fromIntegral new_vertex_size}) (\buffer_region->F.sdl_uploadtogpubuffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (C.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=fromIntegral vertex_size}) (\transfer_buffer_location->FMU.with (C.SDL_GPUBufferRegion {sdl_buffer=index_buffer,sdl_offset=0,sdl_size=fromIntegral new_index_size}) (\buffer_region->F.sdl_uploadtogpubuffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (C.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=fromIntegral size}) (\transfer_buffer_location->FMU.with (C.SDL_GPUBufferRegion {sdl_buffer=parameter_buffer,sdl_offset=0,sdl_size=fromIntegral new_parameter_size}) (\buffer_region->F.sdl_uploadtogpubuffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    F.sdl_endgpucopypass copy_pass
    return (Just ())

load_shader::FP.Ptr T.SDL_GPUDevice->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->String->IO (FP.Ptr T.SDL_GPUShader)
load_shader device format stage num_sampler num_storage_buffer num_uniform_buffer path=do
    shader_code<-DBS.readFile path
    DBS.useAsCStringLen shader_code (\(code,code_size)->FCS.withCString "main" (\entrypoint->FMU.with (C.SDL_GPUShaderCreateInfo {sdl_code_size=fromIntegral code_size,sdl_code=FP.castPtr code,sdl_entrypoint=entrypoint,sdl_format=format,sdl_stage=stage,sdl_num_samplers=num_sampler,sdl_num_storage_textures=0,sdl_num_storage_buffers=num_storage_buffer,sdl_num_uniform_buffers=num_uniform_buffer}) (return_catch_null . F.sdl_creategpushader device)))