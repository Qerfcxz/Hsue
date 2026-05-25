module Engine.Error where

import qualified Control.Monad as CM
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU

catch_error::[Char]->IO FCT.CBool->IO ()
catch_error error_message io_value=do
    value<-io_value
    CM.unless (FMU.toBool value) (error error_message)