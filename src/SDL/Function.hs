{-# LANGUAGE ForeignFunctionInterface #-}

module SDL.Function where

import SDL.Type
import qualified Data.Int as DI
import qualified Data.Word as DW
import qualified Foreign.Ptr as FP
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT

foreign import ccall unsafe "SDL_Init"
    sdl_init::DW.Word32->IO FCT.CBool

foreign import ccall unsafe "SDL_Quit"
    sdl_quit::IO ()

foreign import ccall unsafe "SDL_CreateWindow"
    sdl_createwindow::FCS.CString->FCT.CInt->FCT.CInt->DW.Word64->IO (FP.Ptr SDL_window)

foreign import ccall unsafe "SDL_DestroyWindow"
    sdl_destroywindow::FP.Ptr SDL_window->IO ()

foreign import ccall unsafe "SDL_GetTicks"
    sdl_getticks::IO DW.Word64

foreign import ccall unsafe "SDL_WaitEvent"
    sdl_waitevent::FP.Ptr ()->IO FCT.CBool

foreign import ccall unsafe "SDL_WaitEventTimeout"
    sdl_waiteventtimeout::FP.Ptr ()->DI.Int32->IO FCT.CBool