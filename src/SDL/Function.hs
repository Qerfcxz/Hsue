{-# LANGUAGE ForeignFunctionInterface #-}

module SDL.Function where

import SDL.Constant
import SDL.Type
import qualified Data.Int as DI
import qualified Data.Word as DW
import qualified Foreign.Ptr as FP
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT

foreign import ccall safe "SDL_Init"
    sdl_init::DW.Word32->IO FCT.CBool

foreign import ccall safe "SDL_Quit"
    sdl_quit::IO ()

foreign import ccall safe "SDL_CreateGPUDevice"
    sdl_creategpudevice::DW.Word32->FCT.CBool->FCS.CString->IO (FP.Ptr SDL_GPUDevice)

foreign import ccall safe "SDL_DestroyGPUDevice"
    sdl_destroygpudevice::FP.Ptr SDL_GPUDevice->IO ()

foreign import ccall safe "SDL_ClaimWindowForGPUDevice"
    sdl_claimwindowforgpudevice::FP.Ptr SDL_GPUDevice->FP.Ptr SDL_Window->IO FCT.CBool

foreign import ccall safe "SDL_CreateGPUGraphicsPipeline"
    sdl_creategpugraphicspipeline::FP.Ptr SDL_GPUDevice->FP.Ptr SDL_GPUGraphicsPipelineCreateInfo->IO (FP.Ptr SDL_GPUGraphicsPipeline)

foreign import ccall safe "SDL_AcquireGPUSwapchainTexture"
    sdl_acquiregpuswapchaintexture::FP.Ptr SDL_GPUCommandBuffer->FP.Ptr SDL_Window->FP.Ptr (FP.Ptr SDL_GPUTexture)->FP.Ptr DW.Word32->FP.Ptr DW.Word32->IO FCT.CBool

foreign import ccall safe "SDL_CreateGPUShader"
    sdl_creategpushader::FP.Ptr SDL_GPUDevice->FP.Ptr SDL_GPUShaderCreateInfo->IO (FP.Ptr SDL_GPUShader)

foreign import ccall safe "SDL_ReleaseGPUShader"
    sdl_releasegpushader::FP.Ptr SDL_GPUDevice->FP.Ptr SDL_GPUShader->IO ()

foreign import ccall safe "SDL_ReleaseGPUGraphicsPipeline"
    sdl_releasegpugraphicspipeline::FP.Ptr SDL_GPUDevice->FP.Ptr SDL_GPUGraphicsPipeline->IO ()

foreign import ccall safe "SDL_CreateWindow"
    sdl_createwindow::FCS.CString->FCT.CInt->FCT.CInt->DW.Word64->IO (FP.Ptr SDL_Window)

foreign import ccall safe "SDL_DestroyWindow"
    sdl_destroywindow::FP.Ptr SDL_Window->IO ()

foreign import ccall safe "SDL_WaitEvent"
    sdl_waitevent::FP.Ptr ()->IO FCT.CBool

foreign import ccall safe "SDL_WaitEventTimeout"
    sdl_waiteventtimeout::FP.Ptr ()->DI.Int32->IO FCT.CBool

foreign import ccall unsafe "SDL_BindGPUGraphicsPipeline"
    sdl_bindgpugraphicspipeline::FP.Ptr SDL_GPURenderPass->FP.Ptr SDL_GPUGraphicsPipeline->IO ()

foreign import ccall unsafe "SDL_DrawGPUPrimitives"
    sdl_drawgpuprimitives::FP.Ptr SDL_GPURenderPass->DW.Word32->DW.Word32->DW.Word32->DW.Word32->IO ()

foreign import ccall unsafe "SDL_AcquireGPUCommandBuffer"
    sdl_acquiregpucommandbuffer::FP.Ptr SDL_GPUDevice->IO (FP.Ptr SDL_GPUCommandBuffer)

foreign import ccall unsafe "SDL_SubmitGPUCommandBuffer"
    sdl_submitgpucommandbuffer::FP.Ptr SDL_GPUCommandBuffer->IO FCT.CBool

foreign import ccall unsafe "SDL_BeginGPURenderPass"
    sdl_begingpurenderpass::FP.Ptr SDL_GPUCommandBuffer->FP.Ptr SDL_GPUColorTargetInfo->DW.Word32->FP.Ptr SDL_GPUDepthStencilTargetInfo->IO (FP.Ptr SDL_GPURenderPass)

foreign import ccall unsafe "SDL_EndGPURenderPass"
    sdl_endgpurenderpass::FP.Ptr SDL_GPURenderPass->IO ()

foreign import ccall unsafe "SDL_GetTicks"
    sdl_getticks::IO DW.Word64

foreign import ccall unsafe "SDL_GetWindowID"
    sdl_getwindowid::FP.Ptr SDL_Window->IO DW.Word32