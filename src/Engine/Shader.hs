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
standard_blend_state=C.SDL_GPUColorTargetBlendState {src_color_blendfactor=C.sdl_gpu_blendfactor_src_alpha,dst_color_blendfactor=C.sdl_gpu_blendfactor_one_minus_src_alpha,color_blend_op=C.sdl_gpu_blendop_add,src_alpha_blendfactor=C.sdl_gpu_blendfactor_one,dst_alpha_blendfactor=C.sdl_gpu_blendfactor_zero,alpha_blend_op=C.sdl_gpu_blendop_add,color_write_mask=standard_color_write_mask,enable_blend=FMU.fromBool True,enable_color_write_mask=FMU.fromBool True}

create_triangle_graphics_pipeline::FP.Ptr T.SDL_Window->FP.Ptr T.SDL_GPUDevice->FP.Ptr T.SDL_GPUShader->FP.Ptr T.SDL_GPUShader->IO (FP.Ptr T.SDL_GPUGraphicsPipeline)
create_triangle_graphics_pipeline sdl_window device vertex_shader fragment_shader=FMA.allocaArray 2 $ \vertex_attribute->do
    FS.pokeElemOff vertex_attribute 0 C.SDL_GPUVertexAttribute {location=0,buffer_slot=0,format=C.sdl_gpu_vertexelementformat_float4,offset=0}
    FS.pokeElemOff vertex_attribute 1 C.SDL_GPUVertexAttribute {location=1,buffer_slot=0,format=C.sdl_gpu_vertexelementformat_float2,offset=16}
    FMU.with C.SDL_GPUVertexBufferDescription {slot=0,pitch=24,input_rate=C.sdl_gpu_vertexinputrate_vertex,instance_step_rate=0} $ \vertex_buffer_description->do
        format<-F.sdl_getgpuswapchaintextureformat device sdl_window
        FMU.with C.SDL_GPUColorTargetDescription {format=format,blend_state=standard_blend_state} (\color_target_description->FMU.with C.SDL_GPUGraphicsPipelineCreateInfo {vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_input_state=C.SDL_GPUVertexInputState {vertex_buffer_descriptions=vertex_buffer_description,num_vertex_buffers=1,vertex_attributes=vertex_attribute,num_vertex_attributes=2},primitive_type=C.sdl_gpu_primitivetype_trianglelist,target_info=C.SDL_GPUGraphicsPipelineTargetInfo {color_target_descriptions=color_target_description,num_color_targets=1,has_depth_stencil_target=FMU.fromBool False}} (return_catch_null . F.sdl_creategpugraphicspipeline device))

update_buffer::FP.Ptr T.SDL_GPUDevice->FP.Ptr T.SDL_GPUCommandBuffer->FP.Ptr T.SDL_GPUBuffer->FP.Ptr T.SDL_GPUBuffer->Int->Int->DS.Seq Vertex->DS.Seq DW.Word32->IO (Maybe DW.Word32)
update_buffer device command_buffer vertex_buffer index_buffer max_vertex_size max_index_size vertex index=let vertex_length=DS.length vertex in let index_length=DS.length index in if vertex_length==0||index_length==0 then return Nothing else let vertex_size=vertex_length*FS.sizeOf (undefined::Vertex) in let index_size=index_length*FS.sizeOf (undefined::DW.Word32) in if max_vertex_size<vertex_size||max_index_size<index_size then error "update_buffer: error 1" else let new_vertex_size=fromIntegral vertex_size in let new_index_size=fromIntegral index_size in FMU.with C.SDL_GPUTransferBufferCreateInfo {usage=C.sdl_gpu_transferbufferusage_upload,size=new_vertex_size+new_index_size} $ \transfer_buffer_create_info->do
    transfer_buffer<-F.sdl_creategputransferbuffer device transfer_buffer_create_info
    catch_null transfer_buffer
    map_transfer_buffer<-F.sdl_mapgputransferbuffer device transfer_buffer (FMU.fromBool False)
    catch_null map_transfer_buffer
    seq_poke_array (FP.castPtr map_transfer_buffer) vertex
    seq_poke_array (FP.plusPtr map_transfer_buffer vertex_size) index
    F.sdl_unmapgputransferbuffer device transfer_buffer
    copy_pass<-F.sdl_begingpucopypass command_buffer
    catch_null copy_pass
    FMU.with (C.SDL_GPUTransferBufferLocation {transfer_buffer=transfer_buffer,offset=0}) (\transfer_buffer_location->FMU.with (C.SDL_GPUBufferRegion {buffer=vertex_buffer,offset=0,size=new_vertex_size}) (\buffer_region->F.sdl_uploadtogpubuffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool False)))
    FMU.with (C.SDL_GPUTransferBufferLocation {transfer_buffer=transfer_buffer,offset=new_vertex_size}) (\transfer_buffer_location->FMU.with (C.SDL_GPUBufferRegion {buffer=index_buffer,offset=0,size=new_index_size}) (\buffer_region->F.sdl_uploadtogpubuffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool False)))
    F.sdl_endgpucopypass copy_pass
    F.sdl_releasegputransferbuffer device transfer_buffer
    return (Just (fromIntegral index_length))

load_shader::FP.Ptr T.SDL_GPUDevice->DW.Word32->DW.Word32->DW.Word32->String->IO (FP.Ptr T.SDL_GPUShader)
load_shader device format stage num_uniform_buffers path=do
    shader<-DBS.readFile path
    DBS.useAsCStringLen shader (\(pointer,size)->FCS.withCString "main" (\c_string->FMU.with (C.SDL_GPUShaderCreateInfo {code_size=fromIntegral size,code=FP.castPtr pointer,entrypoint=c_string,format=format,stage=stage,num_samplers=0,num_storage_textures=0,num_storage_buffers=0,num_uniform_buffers=num_uniform_buffers}) (return_catch_null . F.sdl_creategpushader device)))