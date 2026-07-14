{-# LANGUAGE ForeignFunctionInterface #-}

module MSDF.Function where

import MSDF.Type
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall safe "MSDF_Generator"
    msdf_generator::CString->Ptr Word32->CInt->CFloat->CFloat->IO (Ptr MSDF_Output)

foreign import ccall safe "MSDF_Cleaner"
    msdf_cleaner::Ptr MSDF_Output->IO ()