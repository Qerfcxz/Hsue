module Engine.Event where

import Engine.Type
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified Data.Int as DI
import qualified Data.Word as DW
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

get_event::FP.Ptr ()->IO Event
get_event event=do
    value<-F.sdl_waitevent event
    if FMU.toBool value then get_event_a event else error "get_event: error 1"

get_event_time::FP.Ptr ()->DI.Int32->IO Event
get_event_time event time=do
    value<-F.sdl_waiteventtimeout event time
    if FMU.toBool value then get_event_a event else return Time

get_event_a::FP.Ptr ()->IO Event
get_event_a event=do
    event_type<-FS.peekByteOff event 0::IO DW.Word32
    case event_type of
        C.SDL_EVENT_QUIT->return Quit
        _->return Unknown