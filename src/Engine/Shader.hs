{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Engine.Shader where

import Engine.Other
import Engine.Type
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified SDL.Type as T
import qualified Control.Monad as CM
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
standard_blend_state=C.SDL_GPUColorTargetBlendState {src_color_blendfactor=C.sdl_gpu_blendfactor_src_alpha,dst_color_blendfactor=C.sdl_gpu_blendfactor_one_minus_src_alpha,color_blend_op=C.sdl_gpu_blendop_add,src_alpha_blendfactor=C.sdl_gpu_blendfactor_one,dst_alpha_blendfactor=C.sdl_gpu_blendfactor_zero,alpha_blend_op=C.sdl_gpu_blendop_add,color_write_mask=standard_color_write_mask,enable_blend=FMU.fromBool True,enable_color_write_mask=FMU.fromBool False}

create_triangle_graphics_pipeline::FP.Ptr T.SDL_Window->FP.Ptr T.SDL_GPUDevice->FP.Ptr T.SDL_GPUShader->FP.Ptr T.SDL_GPUShader->IO (FP.Ptr T.SDL_GPUGraphicsPipeline)
create_triangle_graphics_pipeline window device vertex_shader fragment_shader=FMA.allocaArray 2 $ \vertex_attribute->do
    FS.pokeElemOff vertex_attribute 0 C.SDL_GPUVertexAttribute {location=0,buffer_slot=0,format=C.sdl_gpu_vertexelementformat_float4,offset=0}
    FS.pokeElemOff vertex_attribute 1 C.SDL_GPUVertexAttribute {location=1,buffer_slot=0,format=C.sdl_gpu_vertexelementformat_float2,offset=16}
    FMU.with C.SDL_GPUVertexBufferDescription {slot=0,pitch=24,input_rate=C.sdl_gpu_vertexinputrate_vertex,instance_step_rate=0} $ \vertex_buffer_description->do
        format<-F.sdl_getgpuswapchaintextureformat device window
        FMU.with C.SDL_GPUColorTargetDescription {format=format,blend_state=standard_blend_state} $ \color_target_description->FMU.with C.SDL_GPUGraphicsPipelineCreateInfo {vertex_shader=vertex_shader,fragment_shader=fragment_shader,vertex_input_state=C.SDL_GPUVertexInputState {vertex_buffer_descriptions=vertex_buffer_description,num_vertex_buffers=1,vertex_attributes=vertex_attribute,num_vertex_attributes=2},primitive_type=C.sdl_gpu_primitivetype_trianglelist,target_info=C.SDL_GPUGraphicsPipelineTargetInfo {color_target_descriptions=color_target_description,num_color_targets=1,has_depth_stencil_target=FMU.fromBool False}} $ \graphics_pipeline_create_info->F.sdl_creategpugraphicspipeline device graphics_pipeline_create_info

create_buffer::FP.Ptr T.SDL_GPUDevice->DS.Seq Vertex->DS.Seq DW.Word32->IO (FP.Ptr T.SDL_GPUBuffer,DW.Word32,DW.Word32)
create_buffer device vertex index=let vertex_size=length vertex*FS.sizeOf (undefined::Vertex) in let index_length=length index in let index_size=index_length*FS.sizeOf (undefined::DW.Word32) in let new_vertex_size=fromIntegral vertex_size in let size=new_vertex_size+fromIntegral index_size in FMU.with (C.SDL_GPUBufferCreateInfo {usage=C.sdl_gpu_bufferusage_vertex DB..|. C.sdl_gpu_bufferusage_index,size=size}) $ \buffer_create_info->FMU.with C.SDL_GPUTransferBufferCreateInfo {usage=C.sdl_gpu_transferbufferusage_upload,size=size} $ \transfer_buffer_create_info->do
    transfer_buffer<-F.sdl_creategputransferbuffer device transfer_buffer_create_info
    map_transfer_buffer<-F.sdl_mapgputransferbuffer device transfer_buffer (FMU.fromBool False)
    CM.when (map_transfer_buffer==FP.nullPtr) (error "create_buffer: error 1")
    seq_poke_array (FP.castPtr map_transfer_buffer) vertex
    seq_poke_array (FP.plusPtr map_transfer_buffer vertex_size) index
    F.sdl_unmapgputransferbuffer device transfer_buffer
    command_buffer<-F.sdl_acquiregpucommandbuffer device
    copy_pass<-F.sdl_begingpucopypass command_buffer
    buffer<-F.sdl_creategpubuffer device buffer_create_info
    FMU.with (C.SDL_GPUTransferBufferLocation {transfer_buffer=transfer_buffer,offset=0}) $ \transfer_buffer_location->FMU.with (C.SDL_GPUBufferRegion {buffer=buffer,offset=0,size=size}) $ \buffer_region->F.sdl_uploadtogpubuffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool False)
    F.sdl_endgpucopypass copy_pass
    catch_error (F.sdl_submitgpucommandbuffer command_buffer)
    F.sdl_releasegputransferbuffer device transfer_buffer
    return (buffer,new_vertex_size,fromIntegral index_length)

load_shader::FP.Ptr T.SDL_GPUDevice->DW.Word32->DW.Word32->String->IO (FP.Ptr T.SDL_GPUShader)
load_shader device format stage path=do
    shader<-DBS.readFile path
    DBS.useAsCStringLen shader $ \(ptr,size)->FCS.withCString "main" $ \cstring->FMU.with (C.SDL_GPUShaderCreateInfo {code_size=fromIntegral size,code=FP.castPtr ptr,entrypoint=cstring,stage=stage,format=format}) $ \shader_create_info->F.sdl_creategpushader device shader_create_info