{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module SDL.Constant where

#include <SDL3/SDL.h>

import SDL.Type
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU

data SDL_GPUBufferBinding=SDL_GPUBufferBinding {buffer::Ptr SDL_GPUBuffer,offset::DW.Word32}

instance Storable SDL_GPUBufferBinding where
    sizeOf _=(#size SDL_GPUBufferBinding)
    alignment _=(#alignment SDL_GPUBufferBinding)
    peek ptr=do
        buffer<-(#peek SDL_GPUBufferBinding,buffer) ptr
        offset<-(#peek SDL_GPUBufferBinding,offset) ptr
        return (SDL_GPUBufferBinding {buffer=buffer,offset=offset})
    poke ptr (SDL_GPUBufferBinding {buffer,offset})=do
        (#poke SDL_GPUBufferBinding,buffer) ptr buffer
        (#poke SDL_GPUBufferBinding,offset) ptr offset

data SDL_GPUColorTargetInfo=SDL_GPUColorTargetInfo {texture::Ptr SDL_GPUTexture,r::FCT.CFloat,g::FCT.CFloat,b::FCT.CFloat,a::FCT.CFloat,load_op::DW.Word32,store_op::DW.Word32}

instance Storable SDL_GPUColorTargetInfo where
    sizeOf _=(#size SDL_GPUColorTargetInfo)
    alignment _=(#alignment SDL_GPUColorTargetInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUColorTargetInfo {texture,r,g,b,a,load_op,store_op})=do
        FMU.fillBytes ptr 0 (#size SDL_GPUColorTargetInfo)
        (#poke SDL_GPUColorTargetInfo,texture) ptr texture
        let new_ptr=(#ptr SDL_GPUColorTargetInfo,clear_color) ptr
        (#poke SDL_FColor,r) new_ptr r
        (#poke SDL_FColor,g) new_ptr g
        (#poke SDL_FColor,b) new_ptr b
        (#poke SDL_FColor,a) new_ptr a
        (#poke SDL_GPUColorTargetInfo,load_op) ptr load_op
        (#poke SDL_GPUColorTargetInfo,store_op) ptr store_op

data SDL_GPUVertexAttribute=SDL_GPUVertexAttribute {location::DW.Word32,buffer_slot::DW.Word32,format::DW.Word32,offset::DW.Word32}

instance Storable SDL_GPUVertexAttribute where
    sizeOf _=(#size SDL_GPUVertexAttribute)
    alignment _=(#alignment SDL_GPUVertexAttribute)
    peek ptr=do
        location<-(#peek SDL_GPUVertexAttribute,location) ptr
        buffer_slot<-(#peek SDL_GPUVertexAttribute,buffer_slot) ptr
        format<-(#peek SDL_GPUVertexAttribute,format) ptr
        offset<-(#peek SDL_GPUVertexAttribute,offset) ptr
        return (SDL_GPUVertexAttribute {location=location,buffer_slot=buffer_slot,format=format,offset=offset})
    poke ptr (SDL_GPUVertexAttribute {location,buffer_slot,format,offset})=do
        (#poke SDL_GPUVertexAttribute,location) ptr location
        (#poke SDL_GPUVertexAttribute,buffer_slot) ptr buffer_slot
        (#poke SDL_GPUVertexAttribute,format) ptr format
        (#poke SDL_GPUVertexAttribute,offset) ptr offset

data SDL_GPUColorTargetDescription=SDL_GPUColorTargetDescription {format::DW.Word32}

instance Storable SDL_GPUColorTargetDescription where
    sizeOf _=(#size SDL_GPUColorTargetDescription)
    alignment _=(#alignment SDL_GPUColorTargetDescription)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUColorTargetDescription {format})=do
        FMU.fillBytes ptr 0 (#size SDL_GPUColorTargetDescription)
        (#poke SDL_GPUColorTargetDescription,format) ptr format

data SDL_GPUShaderCreateInfo=SDL_GPUShaderCreateInfo {code_size::FCT.CSize,code::Ptr DW.Word8,entrypoint::FCS.CString,format::DW.Word32,stage::DW.Word32}

instance Storable SDL_GPUShaderCreateInfo where
    sizeOf _=(#size SDL_GPUShaderCreateInfo)
    alignment _=(#alignment SDL_GPUShaderCreateInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUShaderCreateInfo {code_size,code,entrypoint,format,stage})=do
        FMU.fillBytes ptr 0 (#size SDL_GPUShaderCreateInfo)
        (#poke SDL_GPUShaderCreateInfo,code_size) ptr code_size
        (#poke SDL_GPUShaderCreateInfo,code) ptr code
        (#poke SDL_GPUShaderCreateInfo,entrypoint) ptr entrypoint
        (#poke SDL_GPUShaderCreateInfo,format) ptr format
        (#poke SDL_GPUShaderCreateInfo,stage) ptr stage

data SDL_GPUVertexInputState=SDL_GPUVertexInputState {vertex_attributes::Ptr SDL_GPUVertexAttribute,num_vertex_attributes::DW.Word32}

instance Storable SDL_GPUVertexInputState where
    sizeOf _=(#size SDL_GPUVertexInputState)
    alignment _=(#alignment SDL_GPUVertexInputState)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUVertexInputState {vertex_attributes,num_vertex_attributes})=do
        FMU.fillBytes ptr 0 (#size SDL_GPUVertexInputState)
        (#poke SDL_GPUVertexInputState,vertex_attributes) ptr vertex_attributes
        (#poke SDL_GPUVertexInputState,num_vertex_attributes) ptr num_vertex_attributes

data SDL_GPUGraphicsPipelineTargetInfo=SDL_GPUGraphicsPipelineTargetInfo {color_target_descriptions::Ptr SDL_GPUColorTargetDescription,num_color_targets::DW.Word32,has_depth_stencil_target::FCT.CBool}

instance Storable SDL_GPUGraphicsPipelineTargetInfo where
    sizeOf _=(#size SDL_GPUGraphicsPipelineTargetInfo)
    alignment _=(#alignment SDL_GPUGraphicsPipelineTargetInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUGraphicsPipelineTargetInfo {color_target_descriptions,num_color_targets,has_depth_stencil_target})=do
        FMU.fillBytes ptr 0 (#size SDL_GPUGraphicsPipelineTargetInfo)
        (#poke SDL_GPUGraphicsPipelineTargetInfo,color_target_descriptions) ptr color_target_descriptions
        (#poke SDL_GPUGraphicsPipelineTargetInfo,num_color_targets) ptr num_color_targets
        (#poke SDL_GPUGraphicsPipelineTargetInfo,has_depth_stencil_target) ptr has_depth_stencil_target

data SDL_GPUGraphicsPipelineCreateInfo=SDL_GPUGraphicsPipelineCreateInfo {vertex_shader::Ptr SDL_GPUShader,fragment_shader::Ptr SDL_GPUShader,vertex_input_state::SDL_GPUVertexInputState,primitive_type::DW.Word32,target_info::SDL_GPUGraphicsPipelineTargetInfo}

instance Storable SDL_GPUGraphicsPipelineCreateInfo where
    sizeOf _=(#size SDL_GPUGraphicsPipelineCreateInfo)
    alignment _=(#alignment SDL_GPUGraphicsPipelineCreateInfo)
    peek _=error "peek: error 1"
    poke ptr (SDL_GPUGraphicsPipelineCreateInfo {vertex_shader,fragment_shader,vertex_input_state,primitive_type,target_info})=do
        FMU.fillBytes ptr 0 (#size SDL_GPUGraphicsPipelineCreateInfo)
        (#poke SDL_GPUGraphicsPipelineCreateInfo,vertex_shader) ptr vertex_shader
        (#poke SDL_GPUGraphicsPipelineCreateInfo,fragment_shader) ptr fragment_shader
        let new_ptr=(#ptr SDL_GPUGraphicsPipelineCreateInfo,vertex_input_state) ptr
        poke new_ptr vertex_input_state
        (#poke SDL_GPUGraphicsPipelineCreateInfo,primitive_type) ptr primitive_type
        let new_new_ptr=(#ptr SDL_GPUGraphicsPipelineCreateInfo,target_info) ptr
        poke new_new_ptr target_info

sdl_init_video::DW.Word32
sdl_init_video=(#const SDL_INIT_VIDEO)

sdl_gpu_shaderformat_invalid::DW.Word32
sdl_gpu_shaderformat_invalid=(#const SDL_GPU_SHADERFORMAT_INVALID)

sdl_gpu_shaderformat_dxil::DW.Word32
sdl_gpu_shaderformat_dxil=(#const SDL_GPU_SHADERFORMAT_DXIL)

sdl_gpu_loadop_clear::DW.Word32
sdl_gpu_loadop_clear=(#const SDL_GPU_LOADOP_CLEAR)

sdl_gpu_storeop_store::DW.Word32
sdl_gpu_storeop_store=(#const SDL_GPU_STOREOP_STORE)

sdl_gpu_primitivetype_trianglelist::DW.Word32
sdl_gpu_primitivetype_trianglelist=(#const SDL_GPU_PRIMITIVETYPE_TRIANGLELIST)

sdl_gpu_shaderstage_vertex::DW.Word32
sdl_gpu_shaderstage_vertex=(#const SDL_GPU_SHADERSTAGE_VERTEX)

sdl_gpu_shaderstage_fragment::DW.Word32
sdl_gpu_shaderstage_fragment=(#const SDL_GPU_SHADERSTAGE_FRAGMENT)

sdl_gpu_textureformat_r8g8b8a8_unorm::DW.Word32
sdl_gpu_textureformat_r8g8b8a8_unorm=(#const SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM)

sdl_window_fullscreen::DW.Word64
sdl_window_fullscreen=(#const SDL_WINDOW_FULLSCREEN)

sdl_window_hidden::DW.Word64
sdl_window_hidden=(#const SDL_WINDOW_HIDDEN)

sdl_window_borderless::DW.Word64
sdl_window_borderless=(#const SDL_WINDOW_BORDERLESS)

sdl_window_resizable::DW.Word64
sdl_window_resizable=(#const SDL_WINDOW_RESIZABLE)

sdl_event_size::Int
sdl_event_size=(#size SDL_Event)

sdl_event_type::Ptr ()->IO DW.Word32
sdl_event_type ptr=(#peek SDL_Event,type) ptr

sdl_windowevent_windowid::Ptr ()->IO DW.Word32
sdl_windowevent_windowid ptr=(#peek SDL_WindowEvent,windowID) ptr

sdl_keyboardevent_windowid::Ptr ()->IO DW.Word32
sdl_keyboardevent_windowid ptr=(#peek SDL_KeyboardEvent,windowID) ptr

sdl_keyboardevent_key::Ptr ()->IO DW.Word32
sdl_keyboardevent_key ptr=(#peek SDL_KeyboardEvent,key) ptr

pattern SDL_EVENT_QUIT::DW.Word32
pattern SDL_EVENT_QUIT=(#const SDL_EVENT_QUIT)

pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED::DW.Word32
pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED=(#const SDL_EVENT_WINDOW_CLOSE_REQUESTED)

pattern SDL_EVENT_KEY_UP::DW.Word32
pattern SDL_EVENT_KEY_UP=(#const SDL_EVENT_KEY_UP)

pattern SDL_EVENT_KEY_DOWN::DW.Word32
pattern SDL_EVENT_KEY_DOWN=(#const SDL_EVENT_KEY_DOWN)

pattern SDLK_A::DW.Word32
pattern SDLK_A=(#const SDLK_A)

pattern SDLK_B::DW.Word32
pattern SDLK_B=(#const SDLK_B)

pattern SDLK_C::DW.Word32
pattern SDLK_C=(#const SDLK_C)

pattern SDLK_D::DW.Word32
pattern SDLK_D=(#const SDLK_D)

pattern SDLK_E::DW.Word32
pattern SDLK_E=(#const SDLK_E)

pattern SDLK_F::DW.Word32
pattern SDLK_F=(#const SDLK_F)

pattern SDLK_G::DW.Word32
pattern SDLK_G=(#const SDLK_G)

pattern SDLK_H::DW.Word32
pattern SDLK_H=(#const SDLK_H)

pattern SDLK_I::DW.Word32
pattern SDLK_I=(#const SDLK_I)

pattern SDLK_J::DW.Word32
pattern SDLK_J=(#const SDLK_J)

pattern SDLK_K::DW.Word32
pattern SDLK_K=(#const SDLK_K)

pattern SDLK_L::DW.Word32
pattern SDLK_L=(#const SDLK_L)

pattern SDLK_M::DW.Word32
pattern SDLK_M=(#const SDLK_M)

pattern SDLK_N::DW.Word32
pattern SDLK_N=(#const SDLK_N)

pattern SDLK_O::DW.Word32
pattern SDLK_O=(#const SDLK_O)

pattern SDLK_P::DW.Word32
pattern SDLK_P=(#const SDLK_P)

pattern SDLK_Q::DW.Word32
pattern SDLK_Q=(#const SDLK_Q)

pattern SDLK_R::DW.Word32
pattern SDLK_R=(#const SDLK_R)

pattern SDLK_S::DW.Word32
pattern SDLK_S=(#const SDLK_S)

pattern SDLK_T::DW.Word32
pattern SDLK_T=(#const SDLK_T)

pattern SDLK_U::DW.Word32
pattern SDLK_U=(#const SDLK_U)

pattern SDLK_V::DW.Word32
pattern SDLK_V=(#const SDLK_V)

pattern SDLK_W::DW.Word32
pattern SDLK_W=(#const SDLK_W)

pattern SDLK_X::DW.Word32
pattern SDLK_X=(#const SDLK_X)

pattern SDLK_Y::DW.Word32
pattern SDLK_Y=(#const SDLK_Y)

pattern SDLK_Z::DW.Word32
pattern SDLK_Z=(#const SDLK_Z)