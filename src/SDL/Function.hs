{-# LANGUAGE ForeignFunctionInterface #-}

module SDL.Function where

import SDL.Constant
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
    sdl_sethint::CString->CString->IO CBool

foreign import ccall safe "SDL_CreateGPUDevice"
    sdl_creategpudevice::Word32->CBool->CString->IO (Ptr SDL_GPUDevice)

foreign import ccall safe "SDL_DestroyGPUDevice"
    sdl_destroygpudevice::Ptr SDL_GPUDevice->IO ()

foreign import ccall safe "SDL_ClaimWindowForGPUDevice"
    sdl_claimwindowforgpudevice::Ptr SDL_GPUDevice->Ptr SDL_Window->IO CBool

foreign import ccall safe "SDL_GetGPUSwapchainTextureFormat"
    sdl_getgpuswapchaintextureformat::Ptr SDL_GPUDevice->Ptr SDL_Window->IO Word32

foreign import ccall safe "SDL_CreateGPUGraphicsPipeline"
    sdl_creategpugraphicspipeline::Ptr SDL_GPUDevice->Ptr SDL_GPUGraphicsPipelineCreateInfo->IO (Ptr SDL_GPUGraphicsPipeline)

foreign import ccall safe "SDL_AcquireGPUSwapchainTexture"
    sdl_acquiregpuswapchaintexture::Ptr SDL_GPUCommandBuffer->Ptr SDL_Window->Ptr (Ptr SDL_GPUTexture)->Ptr Word32->Ptr Word32->IO CBool

foreign import ccall safe "SDL_SetGPUSwapchainParameters"
    sdl_setgpuswapchainparameters::Ptr SDL_GPUDevice->Ptr SDL_Window->Word32->Word32->IO CBool

foreign import ccall safe "SDL_CreateGPUShader"
    sdl_creategpushader::Ptr SDL_GPUDevice->Ptr SDL_GPUShaderCreateInfo->IO (Ptr SDL_GPUShader)

foreign import ccall safe "SDL_ReleaseGPUShader"
    sdl_releasegpushader::Ptr SDL_GPUDevice->Ptr SDL_GPUShader->IO ()

foreign import ccall safe "SDL_ReleaseGPUGraphicsPipeline"
    sdl_releasegpugraphicspipeline::Ptr SDL_GPUDevice->Ptr SDL_GPUGraphicsPipeline->IO ()

foreign import ccall safe "SDL_CreateGPUBuffer"
    sdl_creategpubuffer::Ptr SDL_GPUDevice->Ptr SDL_GPUBufferCreateInfo->IO (Ptr SDL_GPUBuffer)

foreign import ccall safe "SDL_ReleaseGPUBuffer"
    sdl_releasegpubuffer::Ptr SDL_GPUDevice->Ptr SDL_GPUBuffer->IO ()

foreign import ccall safe "SDL_CreateGPUTransferBuffer"
    sdl_creategputransferbuffer::Ptr SDL_GPUDevice->Ptr SDL_GPUTransferBufferCreateInfo->IO (Ptr SDL_GPUTransferBuffer)

foreign import ccall safe "SDL_ReleaseGPUTransferBuffer"
    sdl_releasegputransferbuffer::Ptr SDL_GPUDevice->Ptr SDL_GPUTransferBuffer->IO ()

foreign import ccall safe "SDL_MapGPUTransferBuffer"
    sdl_mapgputransferbuffer::Ptr SDL_GPUDevice->Ptr SDL_GPUTransferBuffer->CBool->IO (Ptr ())

foreign import ccall safe "SDL_UnmapGPUTransferBuffer"
    sdl_unmapgputransferbuffer::Ptr SDL_GPUDevice->Ptr SDL_GPUTransferBuffer->IO ()

foreign import ccall safe "SDL_ReleaseWindowFromGPUDevice"
    sdl_releasewindowfromgpudevice::Ptr SDL_GPUDevice->Ptr SDL_Window->IO ()

foreign import ccall safe "SDL_CreateGPUTexture"
    sdl_creategputexture::Ptr SDL_GPUDevice->Ptr SDL_GPUTextureCreateInfo->IO (Ptr SDL_GPUTexture)

foreign import ccall safe "SDL_ReleaseGPUTexture"
    sdl_releasegputexture::Ptr SDL_GPUDevice->Ptr SDL_GPUTexture->IO ()

foreign import ccall safe "SDL_DestroySurface"
    sdl_destroysurface::Ptr SDL_Surface->IO ()

foreign import ccall safe "SDL_ConvertSurface"
    sdl_convertsurface::Ptr SDL_Surface->Word32->IO (Ptr SDL_Surface)

foreign import ccall safe "SDL_CreateGPUSampler"
    sdl_creategpusampler::Ptr SDL_GPUDevice->Ptr SDL_GPUSamplerCreateInfo->IO (Ptr SDL_GPUSampler)

foreign import ccall safe "SDL_ReleaseGPUSampler"
    sdl_releasegpusampler::Ptr SDL_GPUDevice->Ptr SDL_GPUSampler->IO ()

foreign import ccall safe "SDL_WaitForGPUIdle"
    sdl_waitforgpuidle::Ptr SDL_GPUDevice->IO CBool

foreign import ccall safe "SDL_CreateWindow"
    sdl_createwindow::CString->CInt->CInt->Word64->IO (Ptr SDL_Window)

foreign import ccall safe "SDL_DestroyWindow"
    sdl_destroywindow::Ptr SDL_Window->IO ()

foreign import ccall safe "SDL_PushEvent"
    sdl_pushevent::Ptr ()->IO CBool

foreign import ccall safe "SDL_WaitEvent"
    sdl_waitevent::Ptr ()->IO CBool

foreign import ccall safe "SDL_AddTimerNS"
    sdl_addtimerns::Word64->FunPtr (Ptr ()->Word32->Word64->IO Word64)->Ptr ()->IO Word32

foreign import ccall safe "SDL_RemoveTimer"
    sdl_removetimer::Word32->IO CBool

foreign import ccall safe "IMG_Load"
    img_load::CString->IO (Ptr SDL_Surface)

foreign import ccall unsafe "SDL_BindGPUGraphicsPipeline"
    sdl_bindgpugraphicspipeline::Ptr SDL_GPURenderPass->Ptr SDL_GPUGraphicsPipeline->IO ()

foreign import ccall unsafe "SDL_DrawGPUIndexedPrimitives"
    sdl_drawgpuindexedprimitives::Ptr SDL_GPURenderPass->Word32->Word32->Word32->Int32->Word32->IO ()

foreign import ccall unsafe "SDL_AcquireGPUCommandBuffer"
    sdl_acquiregpucommandbuffer::Ptr SDL_GPUDevice->IO (Ptr SDL_GPUCommandBuffer)

foreign import ccall unsafe "SDL_SubmitGPUCommandBuffer"
    sdl_submitgpucommandbuffer::Ptr SDL_GPUCommandBuffer->IO CBool

foreign import ccall unsafe "SDL_BeginGPUCopyPass"
    sdl_begingpucopypass::Ptr SDL_GPUCommandBuffer->IO (Ptr SDL_GPUCopyPass)

foreign import ccall unsafe "SDL_EndGPUCopyPass"
    sdl_endgpucopypass::Ptr SDL_GPUCopyPass->IO ()

foreign import ccall unsafe "SDL_UploadToGPUBuffer"
    sdl_uploadtogpubuffer::Ptr SDL_GPUCopyPass->Ptr SDL_GPUTransferBufferLocation->Ptr SDL_GPUBufferRegion->CBool->IO ()

foreign import ccall unsafe "SDL_BindGPUVertexBuffers"
    sdl_bindgpuvertexbuffers::Ptr SDL_GPURenderPass->Word32->Ptr SDL_GPUBufferBinding->Word32->IO ()

foreign import ccall unsafe "SDL_BindGPUIndexBuffer"
    sdl_bindgpuindexbuffer::Ptr SDL_GPURenderPass->Ptr SDL_GPUBufferBinding->Word32->IO ()

foreign import ccall unsafe "SDL_BeginGPURenderPass"
    sdl_begingpurenderpass::Ptr SDL_GPUCommandBuffer->Ptr SDL_GPUColorTargetInfo->Word32->Ptr SDL_GPUDepthStencilTargetInfo->IO (Ptr SDL_GPURenderPass)

foreign import ccall unsafe "SDL_EndGPURenderPass"
    sdl_endgpurenderpass::Ptr SDL_GPURenderPass->IO ()

foreign import ccall unsafe "SDL_PushGPUVertexUniformData"
    sdl_pushgpuvertexuniformdata::Ptr SDL_GPUCommandBuffer->Word32->Ptr ()->Word32->IO ()

foreign import ccall unsafe "SDL_BindGPUFragmentSamplers"
    sdl_bindgpufragmentsamplers::Ptr SDL_GPURenderPass->Word32->Ptr SDL_GPUTextureSamplerBinding->Word32->IO ()

foreign import ccall unsafe "SDL_CopyGPUTextureToTexture"
    sdl_copygputexturetotexture::Ptr SDL_GPUCopyPass->Ptr SDL_GPUTextureLocation->Ptr SDL_GPUTextureLocation->Word32->Word32->Word32->CBool->IO ()

foreign import ccall unsafe "SDL_UploadToGPUTexture"
    sdl_uploadtogputexture::Ptr SDL_GPUCopyPass->Ptr SDL_GPUTextureTransferInfo->Ptr SDL_GPUTextureRegion->CBool->IO ()

foreign import ccall unsafe "SDL_RegisterEvents"
    sdl_registerevents::CInt->IO Word32

foreign import ccall unsafe "SDL_GetWindowID"
    sdl_getwindowid::Ptr SDL_Window->IO Word32

foreign import ccall "wrapper"
    wrapper::(Ptr ()->Word32->Word64->IO Word64)->IO (FunPtr (Ptr ()->Word32->Word64->IO Word64))