{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Engine where

import Engine.Event
import Engine.Error
import Engine.Type
import qualified SDL.Constant as SC
import qualified SDL.Function as SF
import qualified Data.Int as DI
import qualified Data.IntMap as DIM
import qualified Data.Map as DM
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP

init_engine::IO ()
init_engine=catch_error "init_engine: error 1" (SF.sdl_init SC.sdl_init_video)

quit_engine::IO ()
quit_engine=SF.sdl_quit

create_engine::Maybe DI.Int32->(Engine a->Event->Int)->a->Engine a
create_engine time main_id state=Engine {state=state,leaf=DIM.empty,node=DIM.empty,window=DIM.empty,window_id_map=DM.empty,request=DS.empty,main_id=main_id,time=time}

run_engine::Engine a->IO ()
run_engine engine=FMA.allocaBytes SC.sdl_event_size $ \ptr->case engine.time of
    Nothing->loop_engine ptr engine
    Just time->do
        now<-SF.sdl_getticks
        let new_time=fromIntegral time in loop_engine_time new_time (now+new_time) ptr engine

loop_engine::FP.Ptr ()->Engine a->IO ()
loop_engine ptr engine=do
    new_engine<-run_request engine
    event<-get_event ptr
    case event of
        Quit->return ()
        _->loop_engine ptr (run_event event new_engine)

loop_engine_time::DW.Word64->DW.Word64->FP.Ptr ()->Engine a->IO ()
loop_engine_time time next_time ptr engine=do
    new_engine<-run_request engine
    now<-SF.sdl_getticks
    if now<next_time
        then do
            event<-get_event_time ptr (fromIntegral (next_time-now))
            case event of
                Quit->return ()
                Time->loop_engine_time time (next_time+time) ptr (run_event Time new_engine)
                _->loop_engine_time time next_time ptr (run_event event new_engine)
        else loop_engine_time time (next_time+time) ptr (run_event Time new_engine)

run_request::Engine a->IO (Engine a)
run_request=return

run_event::Event->Engine a->Engine a
run_event _ engine=engine