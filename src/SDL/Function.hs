{-# LANGUAGE ForeignFunctionInterface #-}

module SDL.Function where

import SDL.Include
import SDL.Type
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall safe "SDL_Init"
    sdl_init::Word32->IO CBool

foreign import ccall safe "SDL_Quit"
    sdl_quit::IO ()

foreign import ccall safe "SDL_SetHint"
    sdl_set_hint::CString->CString->IO CBool

foreign import ccall safe "SDL_CreateGPUDevice"
    sdl_create_gpu_device::Word32->CBool->CString->IO (Ptr SDL_GPUDevice)

foreign import ccall safe "SDL_DestroyGPUDevice"
    sdl_destroy_gpu_device::Ptr SDL_GPUDevice->IO ()

foreign import ccall safe "SDL_ClaimWindowForGPUDevice"
    sdl_claim_window_for_gpu_device::Ptr SDL_GPUDevice->Ptr SDL_Window->IO CBool

foreign import ccall safe "SDL_GetGPUSwapchainTextureFormat"
    sdl_get_gpu_swapchain_texture_format::Ptr SDL_GPUDevice->Ptr SDL_Window->IO Word32

foreign import ccall safe "SDL_CreateGPUGraphicsPipeline"
    sdl_create_gpu_graphics_pipeline::Ptr SDL_GPUDevice->Ptr SDL_GPUGraphicsPipelineCreateInfo->IO (Ptr SDL_GPUGraphicsPipeline)

foreign import ccall safe "SDL_AcquireGPUSwapchainTexture"
    sdl_acquire_gpu_swapchain_texture::Ptr SDL_GPUCommandBuffer->Ptr SDL_Window->Ptr (Ptr SDL_GPUTexture)->Ptr Word32->Ptr Word32->IO CBool

foreign import ccall safe "SDL_SetGPUSwapchainParameters"
    sdl_set_gpu_swapchain_parameters::Ptr SDL_GPUDevice->Ptr SDL_Window->Word32->Word32->IO CBool

foreign import ccall safe "SDL_CreateGPUShader"
    sdl_create_gpu_shader::Ptr SDL_GPUDevice->Ptr SDL_GPUShaderCreateInfo->IO (Ptr SDL_GPUShader)

foreign import ccall safe "SDL_ReleaseGPUShader"
    sdl_release_gpu_shader::Ptr SDL_GPUDevice->Ptr SDL_GPUShader->IO ()

foreign import ccall safe "SDL_ReleaseGPUGraphicsPipeline"
    sdl_release_gpu_graphics_pipeline::Ptr SDL_GPUDevice->Ptr SDL_GPUGraphicsPipeline->IO ()

foreign import ccall safe "SDL_CreateGPUBuffer"
    sdl_create_gpu_buffer::Ptr SDL_GPUDevice->Ptr SDL_GPUBufferCreateInfo->IO (Ptr SDL_GPUBuffer)

foreign import ccall safe "SDL_ReleaseGPUBuffer"
    sdl_release_gpu_buffer::Ptr SDL_GPUDevice->Ptr SDL_GPUBuffer->IO ()

foreign import ccall safe "SDL_CreateGPUTransferBuffer"
    sdl_create_gpu_transfer_buffer::Ptr SDL_GPUDevice->Ptr SDL_GPUTransferBufferCreateInfo->IO (Ptr SDL_GPUTransferBuffer)

foreign import ccall safe "SDL_ReleaseGPUTransferBuffer"
    sdl_release_gpu_transfer_buffer::Ptr SDL_GPUDevice->Ptr SDL_GPUTransferBuffer->IO ()

foreign import ccall safe "SDL_MapGPUTransferBuffer"
    sdl_map_gpu_transfer_buffer::Ptr SDL_GPUDevice->Ptr SDL_GPUTransferBuffer->CBool->IO (Ptr ())

foreign import ccall safe "SDL_UnmapGPUTransferBuffer"
    sdl_unmap_gpu_transfer_buffer::Ptr SDL_GPUDevice->Ptr SDL_GPUTransferBuffer->IO ()

foreign import ccall safe "SDL_ReleaseWindowFromGPUDevice"
    sdl_release_window_from_gpu_device::Ptr SDL_GPUDevice->Ptr SDL_Window->IO ()

foreign import ccall safe "SDL_CreateGPUTexture"
    sdl_create_gpu_texture::Ptr SDL_GPUDevice->Ptr SDL_GPUTextureCreateInfo->IO (Ptr SDL_GPUTexture)

foreign import ccall safe "SDL_ReleaseGPUTexture"
    sdl_release_gpu_texture::Ptr SDL_GPUDevice->Ptr SDL_GPUTexture->IO ()

foreign import ccall safe "SDL_DestroySurface"
    sdl_destroy_surface::Ptr SDL_Surface->IO ()

foreign import ccall safe "SDL_ConvertSurface"
    sdl_convert_surface::Ptr SDL_Surface->Word32->IO (Ptr SDL_Surface)

foreign import ccall safe "SDL_CreateGPUSampler"
    sdl_create_gpu_sampler::Ptr SDL_GPUDevice->Ptr SDL_GPUSamplerCreateInfo->IO (Ptr SDL_GPUSampler)

foreign import ccall safe "SDL_ReleaseGPUSampler"
    sdl_release_gpu_sampler::Ptr SDL_GPUDevice->Ptr SDL_GPUSampler->IO ()

foreign import ccall safe "SDL_WaitForGPUIdle"
    sdl_wait_for_gpu_idle::Ptr SDL_GPUDevice->IO CBool

foreign import ccall safe "SDL_CreateWindow"
    sdl_create_window::CString->CInt->CInt->Word64->IO (Ptr SDL_Window)

foreign import ccall safe "SDL_DestroyWindow"
    sdl_destroy_window::Ptr SDL_Window->IO ()

foreign import ccall safe "SDL_PushEvent"
    sdl_push_event::Ptr ()->IO CBool

foreign import ccall safe "SDL_WaitEvent"
    sdl_wait_event::Ptr ()->IO CBool

foreign import ccall safe "SDL_AddTimerNS"
    sdl_add_timer_ns::Word64->FunPtr (Ptr ()->Word32->Word64->IO Word64)->Ptr ()->IO Word32

foreign import ccall safe "SDL_RemoveTimer"
    sdl_remove_timer::Word32->IO CBool

foreign import ccall safe "SDL_GetClipboardText"
    sdl_get_clipboard_text::IO CString

foreign import ccall safe "SDL_HasClipboardText"
    sdl_has_clipboard_text::IO CBool

foreign import ccall safe "SDL_SetClipboardText"
    sdl_set_clipboard_text::CString->IO CBool

foreign import ccall safe "SDL_free"
    sdl_free::Ptr ()->IO ()

foreign import ccall safe "IMG_Load"
    img_load::CString->IO (Ptr SDL_Surface)

foreign import ccall safe "IMG_LoadAnimation"
    img_load_animation::CString->IO (Ptr IMG_Animation)

foreign import ccall safe "IMG_FreeAnimation"
    img_free_animation::Ptr IMG_Animation->IO ()

foreign import ccall unsafe "SDL_BindGPUGraphicsPipeline"
    sdl_bind_gpu_graphics_pipeline::Ptr SDL_GPURenderPass->Ptr SDL_GPUGraphicsPipeline->IO ()

foreign import ccall unsafe "SDL_DrawGPUIndexedPrimitives"
    sdl_draw_gpu_indexed_primitives::Ptr SDL_GPURenderPass->Word32->Word32->Word32->Int32->Word32->IO ()

foreign import ccall unsafe "SDL_DrawGPUPrimitives"
    sdl_draw_gpu_primitives::Ptr SDL_GPURenderPass->Word32->Word32->Word32->Word32->IO ()

foreign import ccall unsafe "SDL_AcquireGPUCommandBuffer"
    sdl_acquire_gpu_command_buffer::Ptr SDL_GPUDevice->IO (Ptr SDL_GPUCommandBuffer)

foreign import ccall unsafe "SDL_SubmitGPUCommandBuffer"
    sdl_submit_gpu_command_buffer::Ptr SDL_GPUCommandBuffer->IO CBool

foreign import ccall unsafe "SDL_CancelGPUCommandBuffer"
    sdl_cancel_gpu_command_buffer::Ptr SDL_GPUCommandBuffer->IO CBool

foreign import ccall unsafe "SDL_BeginGPUCopyPass"
    sdl_begin_gpu_copy_pass::Ptr SDL_GPUCommandBuffer->IO (Ptr SDL_GPUCopyPass)

foreign import ccall unsafe "SDL_EndGPUCopyPass"
    sdl_end_gpu_copy_pass::Ptr SDL_GPUCopyPass->IO ()

foreign import ccall unsafe "SDL_UploadToGPUBuffer"
    sdl_upload_to_gpu_buffer::Ptr SDL_GPUCopyPass->Ptr SDL_GPUTransferBufferLocation->Ptr SDL_GPUBufferRegion->CBool->IO ()

foreign import ccall unsafe "SDL_BindGPUVertexBuffers"
    sdl_bind_gpu_vertex_buffers::Ptr SDL_GPURenderPass->Word32->Ptr SDL_GPUBufferBinding->Word32->IO ()

foreign import ccall unsafe "SDL_BindGPUIndexBuffer"
    sdl_bind_gpu_index_buffer::Ptr SDL_GPURenderPass->Ptr SDL_GPUBufferBinding->Word32->IO ()

foreign import ccall unsafe "SDL_BindGPUVertexStorageBuffers"
    sdl_bind_gpu_vertex_storage_buffers::Ptr SDL_GPURenderPass->Word32->Ptr (Ptr SDL_GPUBuffer)->Word32->IO ()

foreign import ccall unsafe "SDL_BeginGPURenderPass"
    sdl_begin_gpu_render_pass::Ptr SDL_GPUCommandBuffer->Ptr SDL_GPUColorTargetInfo->Word32->Ptr SDL_GPUDepthStencilTargetInfo->IO (Ptr SDL_GPURenderPass)

foreign import ccall unsafe "SDL_EndGPURenderPass"
    sdl_end_gpu_render_pass::Ptr SDL_GPURenderPass->IO ()

foreign import ccall unsafe "SDL_PushGPUVertexUniformData"
    sdl_push_gpu_vertex_uniform_data::Ptr SDL_GPUCommandBuffer->Word32->Ptr ()->Word32->IO ()

foreign import ccall unsafe "SDL_PushGPUFragmentUniformData"
    sdl_push_gpu_fragment_uniform_data::Ptr SDL_GPUCommandBuffer->Word32->Ptr ()->Word32->IO ()

foreign import ccall unsafe "SDL_BindGPUFragmentSamplers"
    sdl_bind_gpu_fragment_samplers::Ptr SDL_GPURenderPass->Word32->Ptr SDL_GPUTextureSamplerBinding->Word32->IO ()

foreign import ccall unsafe "SDL_CopyGPUTextureToTexture"
    sdl_copy_gpu_texture_to_texture::Ptr SDL_GPUCopyPass->Ptr SDL_GPUTextureLocation->Ptr SDL_GPUTextureLocation->Word32->Word32->Word32->CBool->IO ()

foreign import ccall unsafe "SDL_UploadToGPUTexture"
    sdl_upload_to_gpu_texture::Ptr SDL_GPUCopyPass->Ptr SDL_GPUTextureTransferInfo->Ptr SDL_GPUTextureRegion->CBool->IO ()

foreign import ccall unsafe "SDL_RegisterEvents"
    sdl_register_events::CInt->IO Word32

foreign import ccall unsafe "SDL_GetWindowID"
    sdl_get_window_id::Ptr SDL_Window->IO Word32

foreign import ccall "wrapper"
    wrapper::(Ptr ()->Word32->Word64->IO Word64)->IO (FunPtr (Ptr ()->Word32->Word64->IO Word64))