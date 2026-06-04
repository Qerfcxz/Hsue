{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module SDL.Constant where

#include <SDL3/SDL.h>

import SDL.Type
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data SDL_FColor=SDL_FColor {r::CFloat,g::CFloat,b::CFloat,a::CFloat}

instance Storable SDL_FColor where
    sizeOf _=(#size SDL_FColor)
    alignment _=(#alignment SDL_FColor)
    peek _=error "peek: error 1"
    poke ptr (SDL_FColor {r,g,b,a})=do
        fillBytes ptr 0 (#size SDL_FColor)
        (#poke SDL_FColor,r) ptr r
        (#poke SDL_FColor,g) ptr g
        (#poke SDL_FColor,b) ptr b
        (#poke SDL_FColor,a) ptr a

data SDL_GPUBufferBinding=SDL_GPUBufferBinding {buffer::Ptr SDL_GPUBuffer,offset::Word32}

instance Storable SDL_GPUBufferBinding where
    sizeOf _=(#size SDL_GPUBufferBinding)
    alignment _=(#alignment SDL_GPUBufferBinding)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUBufferBinding {buffer,offset})=do
        fillBytes ptr 0 (#size SDL_GPUBufferBinding)
        (#poke SDL_GPUBufferBinding,buffer) ptr buffer
        (#poke SDL_GPUBufferBinding,offset) ptr offset

data SDL_GPUColorTargetInfo=SDL_GPUColorTargetInfo {texture::Ptr SDL_GPUTexture,clear_color::SDL_FColor,load_op::Word32,store_op::Word32}

instance Storable SDL_GPUColorTargetInfo where
    sizeOf _=(#size SDL_GPUColorTargetInfo)
    alignment _=(#alignment SDL_GPUColorTargetInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUColorTargetInfo {texture,clear_color,load_op,store_op})=do
        fillBytes ptr 0 (#size SDL_GPUColorTargetInfo)
        (#poke SDL_GPUColorTargetInfo,texture) ptr texture
        (#poke SDL_GPUColorTargetInfo,clear_color) ptr clear_color
        (#poke SDL_GPUColorTargetInfo,load_op) ptr load_op
        (#poke SDL_GPUColorTargetInfo,store_op) ptr store_op

data SDL_GPUVertexAttribute=SDL_GPUVertexAttribute {location::Word32,buffer_slot::Word32,format::Word32,offset::Word32}

instance Storable SDL_GPUVertexAttribute where
    sizeOf _=(#size SDL_GPUVertexAttribute)
    alignment _=(#alignment SDL_GPUVertexAttribute)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUVertexAttribute {location,buffer_slot,format,offset})=do
        fillBytes ptr 0 (#size SDL_GPUVertexAttribute)
        (#poke SDL_GPUVertexAttribute,location) ptr location
        (#poke SDL_GPUVertexAttribute,buffer_slot) ptr buffer_slot
        (#poke SDL_GPUVertexAttribute,format) ptr format
        (#poke SDL_GPUVertexAttribute,offset) ptr offset

data SDL_GPUColorTargetDescription=SDL_GPUColorTargetDescription {format::Word32,blend_state::SDL_GPUColorTargetBlendState}

instance Storable SDL_GPUColorTargetDescription where
    sizeOf _=(#size SDL_GPUColorTargetDescription)
    alignment _=(#alignment SDL_GPUColorTargetDescription)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUColorTargetDescription {format,blend_state})=do
        fillBytes ptr 0 (#size SDL_GPUColorTargetDescription)
        (#poke SDL_GPUColorTargetDescription,format) ptr format
        (#poke SDL_GPUColorTargetDescription,blend_state) ptr blend_state

data SDL_GPUColorTargetBlendState=SDL_GPUColorTargetBlendState {src_color_blendfactor::Word32,dst_color_blendfactor::Word32,color_blend_op::Word32,src_alpha_blendfactor::Word32,dst_alpha_blendfactor::Word32,alpha_blend_op::Word32,color_write_mask::Word8,enable_blend::CBool,enable_color_write_mask::CBool}

instance Storable SDL_GPUColorTargetBlendState where
    sizeOf _=(#size SDL_GPUColorTargetBlendState)
    alignment _=(#alignment SDL_GPUColorTargetBlendState)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUColorTargetBlendState {src_color_blendfactor,dst_color_blendfactor,color_blend_op,src_alpha_blendfactor,dst_alpha_blendfactor,alpha_blend_op,color_write_mask,enable_blend,enable_color_write_mask})=do
        fillBytes ptr 0 (#size SDL_GPUColorTargetBlendState)
        (#poke SDL_GPUColorTargetBlendState,src_color_blendfactor) ptr src_color_blendfactor
        (#poke SDL_GPUColorTargetBlendState,dst_color_blendfactor) ptr dst_color_blendfactor
        (#poke SDL_GPUColorTargetBlendState,color_blend_op) ptr color_blend_op
        (#poke SDL_GPUColorTargetBlendState,src_alpha_blendfactor) ptr src_alpha_blendfactor
        (#poke SDL_GPUColorTargetBlendState,dst_alpha_blendfactor) ptr dst_alpha_blendfactor
        (#poke SDL_GPUColorTargetBlendState,alpha_blend_op) ptr alpha_blend_op
        (#poke SDL_GPUColorTargetBlendState,color_write_mask) ptr color_write_mask
        (#poke SDL_GPUColorTargetBlendState,enable_blend) ptr enable_blend
        (#poke SDL_GPUColorTargetBlendState,enable_color_write_mask) ptr enable_color_write_mask

data SDL_GPUShaderCreateInfo=SDL_GPUShaderCreateInfo {code_size::CSize,code::Ptr Word8,entrypoint::CString,format::Word32,stage::Word32}

instance Storable SDL_GPUShaderCreateInfo where
    sizeOf _=(#size SDL_GPUShaderCreateInfo)
    alignment _=(#alignment SDL_GPUShaderCreateInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUShaderCreateInfo {code_size,code,entrypoint,format,stage})=do
        fillBytes ptr 0 (#size SDL_GPUShaderCreateInfo)
        (#poke SDL_GPUShaderCreateInfo,code_size) ptr code_size
        (#poke SDL_GPUShaderCreateInfo,code) ptr code
        (#poke SDL_GPUShaderCreateInfo,entrypoint) ptr entrypoint
        (#poke SDL_GPUShaderCreateInfo,format) ptr format
        (#poke SDL_GPUShaderCreateInfo,stage) ptr stage

data SDL_GPUVertexInputState=SDL_GPUVertexInputState {vertex_buffer_descriptions::Ptr SDL_GPUVertexBufferDescription,num_vertex_buffers::Word32,vertex_attributes::Ptr SDL_GPUVertexAttribute,num_vertex_attributes::Word32}

instance Storable SDL_GPUVertexInputState where
    sizeOf _=(#size SDL_GPUVertexInputState)
    alignment _=(#alignment SDL_GPUVertexInputState)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUVertexInputState {vertex_buffer_descriptions,num_vertex_buffers,vertex_attributes,num_vertex_attributes})=do
        fillBytes ptr 0 (#size SDL_GPUVertexInputState)
        (#poke SDL_GPUVertexInputState,vertex_buffer_descriptions) ptr vertex_buffer_descriptions
        (#poke SDL_GPUVertexInputState,num_vertex_buffers) ptr num_vertex_buffers
        (#poke SDL_GPUVertexInputState,vertex_attributes) ptr vertex_attributes
        (#poke SDL_GPUVertexInputState,num_vertex_attributes) ptr num_vertex_attributes

data SDL_GPUBufferCreateInfo=SDL_GPUBufferCreateInfo {usage::Word32,size::Word32}

instance Storable SDL_GPUBufferCreateInfo where
    sizeOf _=(#size SDL_GPUBufferCreateInfo)
    alignment _=(#alignment SDL_GPUBufferCreateInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUBufferCreateInfo {usage,size})=do
        fillBytes ptr 0 (#size SDL_GPUBufferCreateInfo)
        (#poke SDL_GPUBufferCreateInfo,usage) ptr usage
        (#poke SDL_GPUBufferCreateInfo,size) ptr size

data SDL_GPUTransferBufferCreateInfo=SDL_GPUTransferBufferCreateInfo {usage::Word32,size::Word32}

instance Storable SDL_GPUTransferBufferCreateInfo where
    sizeOf _=(#size SDL_GPUTransferBufferCreateInfo)
    alignment _=(#alignment SDL_GPUTransferBufferCreateInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUTransferBufferCreateInfo {usage,size})=do
        fillBytes ptr 0 (#size SDL_GPUTransferBufferCreateInfo)
        (#poke SDL_GPUTransferBufferCreateInfo,usage) ptr usage
        (#poke SDL_GPUTransferBufferCreateInfo,size) ptr size

data SDL_GPUTransferBufferLocation=SDL_GPUTransferBufferLocation {transfer_buffer::Ptr SDL_GPUTransferBuffer,offset::Word32}

instance Storable SDL_GPUTransferBufferLocation where
    sizeOf _=(#size SDL_GPUTransferBufferLocation)
    alignment _=(#alignment SDL_GPUTransferBufferLocation)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUTransferBufferLocation {transfer_buffer,offset})=do
        fillBytes ptr 0 (#size SDL_GPUTransferBufferLocation)
        (#poke SDL_GPUTransferBufferLocation,transfer_buffer) ptr transfer_buffer
        (#poke SDL_GPUTransferBufferLocation,offset) ptr offset

data SDL_GPUBufferRegion=SDL_GPUBufferRegion {buffer::Ptr SDL_GPUBuffer,offset::Word32,size::Word32}

instance Storable SDL_GPUBufferRegion where
    sizeOf _=(#size SDL_GPUBufferRegion)
    alignment _=(#alignment SDL_GPUBufferRegion)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUBufferRegion {buffer,offset,size})=do
        fillBytes ptr 0 (#size SDL_GPUBufferRegion)
        (#poke SDL_GPUBufferRegion,buffer) ptr buffer
        (#poke SDL_GPUBufferRegion,offset) ptr offset
        (#poke SDL_GPUBufferRegion,size) ptr size

data SDL_GPUVertexBufferDescription=SDL_GPUVertexBufferDescription {slot::Word32,pitch::Word32,input_rate::Word32,instance_step_rate::Word32}

instance Storable SDL_GPUVertexBufferDescription where
    sizeOf _=(#size SDL_GPUVertexBufferDescription)
    alignment _=(#alignment SDL_GPUVertexBufferDescription)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUVertexBufferDescription {slot,pitch,input_rate,instance_step_rate})=do
        fillBytes ptr 0 (#size SDL_GPUVertexBufferDescription)
        (#poke SDL_GPUVertexBufferDescription,slot) ptr slot
        (#poke SDL_GPUVertexBufferDescription,pitch) ptr pitch
        (#poke SDL_GPUVertexBufferDescription,input_rate) ptr input_rate
        (#poke SDL_GPUVertexBufferDescription,instance_step_rate) ptr instance_step_rate

data SDL_GPUGraphicsPipelineTargetInfo=SDL_GPUGraphicsPipelineTargetInfo {color_target_descriptions::Ptr SDL_GPUColorTargetDescription,num_color_targets::Word32,has_depth_stencil_target::CBool}

instance Storable SDL_GPUGraphicsPipelineTargetInfo where
    sizeOf _=(#size SDL_GPUGraphicsPipelineTargetInfo)
    alignment _=(#alignment SDL_GPUGraphicsPipelineTargetInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUGraphicsPipelineTargetInfo {color_target_descriptions,num_color_targets,has_depth_stencil_target})=do
        fillBytes ptr 0 (#size SDL_GPUGraphicsPipelineTargetInfo)
        (#poke SDL_GPUGraphicsPipelineTargetInfo,color_target_descriptions) ptr color_target_descriptions
        (#poke SDL_GPUGraphicsPipelineTargetInfo,num_color_targets) ptr num_color_targets
        (#poke SDL_GPUGraphicsPipelineTargetInfo,has_depth_stencil_target) ptr has_depth_stencil_target

data SDL_GPUGraphicsPipelineCreateInfo=SDL_GPUGraphicsPipelineCreateInfo {vertex_shader::Ptr SDL_GPUShader,fragment_shader::Ptr SDL_GPUShader,vertex_input_state::SDL_GPUVertexInputState,primitive_type::Word32,target_info::SDL_GPUGraphicsPipelineTargetInfo}

instance Storable SDL_GPUGraphicsPipelineCreateInfo where
    sizeOf _=(#size SDL_GPUGraphicsPipelineCreateInfo)
    alignment _=(#alignment SDL_GPUGraphicsPipelineCreateInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUGraphicsPipelineCreateInfo {vertex_shader,fragment_shader,vertex_input_state,primitive_type,target_info})=do
        fillBytes ptr 0 (#size SDL_GPUGraphicsPipelineCreateInfo)
        (#poke SDL_GPUGraphicsPipelineCreateInfo,vertex_shader) ptr vertex_shader
        (#poke SDL_GPUGraphicsPipelineCreateInfo,fragment_shader) ptr fragment_shader
        (#poke SDL_GPUGraphicsPipelineCreateInfo,vertex_input_state) ptr vertex_input_state
        (#poke SDL_GPUGraphicsPipelineCreateInfo,primitive_type) ptr primitive_type
        (#poke SDL_GPUGraphicsPipelineCreateInfo,target_info) ptr target_info

sdl_init_video::Word32
sdl_init_video=(#const SDL_INIT_VIDEO)

sdl_gpu_shaderformat_invalid::Word32
sdl_gpu_shaderformat_invalid=(#const SDL_GPU_SHADERFORMAT_INVALID)

sdl_gpu_shaderformat_dxil::Word32
sdl_gpu_shaderformat_dxil=(#const SDL_GPU_SHADERFORMAT_DXIL)

sdl_gpu_loadop_clear::Word32
sdl_gpu_loadop_clear=(#const SDL_GPU_LOADOP_CLEAR)

sdl_gpu_loadop_load::Word32
sdl_gpu_loadop_load=(#const SDL_GPU_LOADOP_LOAD)

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

sdl_gpu_textureformat_r8g8b8a8_unorm::Word32
sdl_gpu_textureformat_r8g8b8a8_unorm=(#const SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM)

sdl_gpu_textureformat_b8g8r8a8_unorm::Word32
sdl_gpu_textureformat_b8g8r8a8_unorm=(#const SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM)

sdl_gpu_bufferusage_vertex::Word32
sdl_gpu_bufferusage_vertex=(#const SDL_GPU_BUFFERUSAGE_VERTEX)

sdl_gpu_bufferusage_index::Word32
sdl_gpu_bufferusage_index=(#const SDL_GPU_BUFFERUSAGE_INDEX)

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

sdl_window_fullscreen::Word64
sdl_window_fullscreen=(#const SDL_WINDOW_FULLSCREEN)

sdl_window_hidden::Word64
sdl_window_hidden=(#const SDL_WINDOW_HIDDEN)

sdl_window_borderless::Word64
sdl_window_borderless=(#const SDL_WINDOW_BORDERLESS)

sdl_window_resizable::Word64
sdl_window_resizable=(#const SDL_WINDOW_RESIZABLE)

sdl_event_size::Int
sdl_event_size=(#size SDL_Event)

sdl_event_alignment::Int
sdl_event_alignment=(#alignment SDL_Event)

sdl_event_type::Ptr ()->IO Word32
sdl_event_type ptr=(#peek SDL_Event,type) ptr

sdl_windowevent_windowid::Ptr ()->IO Word32
sdl_windowevent_windowid ptr=(#peek SDL_WindowEvent,windowID) ptr

sdl_keyboardevent_windowid::Ptr ()->IO Word32
sdl_keyboardevent_windowid ptr=(#peek SDL_KeyboardEvent,windowID) ptr

sdl_keyboardevent_key::Ptr ()->IO Word32
sdl_keyboardevent_key ptr=(#peek SDL_KeyboardEvent,key) ptr

pattern SDL_EVENT_QUIT::Word32
pattern SDL_EVENT_QUIT=(#const SDL_EVENT_QUIT)

pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED::Word32
pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED=(#const SDL_EVENT_WINDOW_CLOSE_REQUESTED)

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