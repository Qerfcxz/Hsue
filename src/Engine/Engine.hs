{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Engine where

import Engine.Event
import Engine.Error
import Engine.Request
import Engine.Type
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified Data.Int as DI
import qualified Data.IntMap as DIM
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Word as DW
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP

init_engine::IO ()
init_engine=catch_error "init_engine: error 1" (F.sdl_init C.sdl_init_video)

quit_engine::IO ()
quit_engine=F.sdl_quit

create_engine::Maybe DI.Int32->(Engine a->Event->Maybe Int)->a->Engine a
create_engine timer main_id state=case timer of
    Nothing->Engine {state=state,leaf=DIM.empty,node=DIM.empty,window=DIM.empty,window_map=DM.empty,request=DSeq.empty,key=DSet.empty,main_id=main_id,timer=Keep_off}
    Just time->Engine {state=state,leaf=DIM.empty,node=DIM.empty,window=DIM.empty,window_map=DM.empty,request=DSeq.empty,key=DSet.empty,main_id=main_id,timer=Keep_on {time=fromIntegral time}}

run_engine::Engine a->IO ()
run_engine engine=FMA.allocaBytes C.sdl_event_size $ \ptr->case engine.timer of
    Keep_off->loop_engine ptr engine
    Keep_on {time}->do
        now<-F.sdl_getticks
        loop_engine_time time (now+time) ptr engine
    _->error "run_engine: error 1"

loop_engine::FP.Ptr ()->Engine a->IO ()
loop_engine ptr engine=do
    new_engine<-run_request engine
    (event,key)<-get_event ptr engine.window_map engine.key
    case event of
        Quit->return ()
        _->let new_new_engine=run_event event (new_engine {key=key}) in case new_new_engine.timer of
            Keep_off->loop_engine ptr new_new_engine
            Turn_on {time}->do
                now<-F.sdl_getticks
                loop_engine_time time (now+time) ptr (new_new_engine {timer=Keep_on {time=time}})
            _->error "loop_engine: error 1"

loop_engine_time::DW.Word64->DW.Word64->FP.Ptr ()->Engine a->IO ()
loop_engine_time time next_time ptr engine=do
    new_engine<-run_request engine
    now<-F.sdl_getticks
    if now<next_time
        then do
            (event,key)<-get_event_time (fromIntegral (next_time-now)) ptr engine.window_map engine.key
            case event of
                Quit->return ()
                Time->loop_engine_time_a (next_time+time) ptr (run_event Time (new_engine {key=key}))
                _->loop_engine_time_a next_time ptr (run_event event (new_engine {key=key}))
        else loop_engine_time_a (max (next_time+time) (now+time)) ptr (run_event Time new_engine)

loop_engine_time_a::DW.Word64->FP.Ptr ()->Engine a->IO ()
loop_engine_time_a next_time ptr engine=case engine.timer of
    Keep_on {time}->loop_engine_time time next_time ptr engine
    Turn_off->loop_engine ptr (engine {timer=Keep_off})
    Turn_on {time}->do
        now<-F.sdl_getticks
        loop_engine_time time (now+time) ptr (engine {timer=Keep_on {time=time}})
    _->error "loop_engine_time_a: error 1"

run_request::Engine a->IO (Engine a)
run_request engine=case engine.request of
    DSeq.Empty->return engine
    (request DSeq.:<| other_request)->do
        new_engine<-do_request request (engine {request=other_request})
        run_request new_engine

run_event::Event->Engine a->Engine a
run_event event engine=case engine.main_id engine event of
    Nothing->engine
    Just main_id->run_event_a main_id DIM.empty event engine

run_event_a::Int->DIM.IntMap Event->Event->Engine a->Engine a
run_event_a leaf_id cache event engine=case DIM.lookup leaf_id engine.leaf of
    Nothing->error "run_event_a: error 1"
    Just leaf->case leaf.father of
        Nothing->run_event_b leaf cache event event engine
        Just node_id->case DIM.lookup node_id cache of
            Nothing->let engine_node=engine.node in case DIM.lookup node_id engine_node of
                Nothing->error "run_event_a: error 2"
                Just node->let (seq_node_id,new_event)=run_event_c cache node.ancestry (DSeq.singleton node_id) event in let (new_cache,new_new_event)=run_event_d engine engine_node seq_node_id cache new_event in run_event_b leaf new_cache new_new_event event engine
            Just new_event->run_event_b leaf cache new_event event engine

run_event_b::Leaf a->DIM.IntMap Event->Event->Event->Engine a->Engine a
run_event_b leaf cache new_event event engine=let new_engine=run_widget new_event leaf.widget engine in case leaf.next new_engine new_event of
    Nothing->new_engine
    Just new_leaf_id->run_event_a new_leaf_id cache event new_engine

run_event_c::DIM.IntMap Event->DSeq.Seq Int->DSeq.Seq Int->Event->(DSeq.Seq Int,Event)
run_event_c _ DSeq.Empty seq_node_id event=(seq_node_id,event)
run_event_c cache (other_node_id DSeq.:|> node_id) seq_node_id event=let maybe_event=DIM.lookup node_id cache in case maybe_event of
    Nothing->run_event_c cache other_node_id (node_id DSeq.<| seq_node_id) event
    Just new_event->(seq_node_id,new_event)

run_event_d::Engine a->DIM.IntMap (Node a)->DSeq.Seq Int->DIM.IntMap Event->Event->(DIM.IntMap Event,Event)
run_event_d _ _ DSeq.Empty cache event=(cache,event)
run_event_d engine engine_node (node_id DSeq.:<| other_node_id) cache event=case DIM.lookup node_id engine_node of
    Nothing->error "run_event_d: error 1"
    Just node->let new_event=node.event_transform engine event in run_event_d engine engine_node other_node_id (DIM.insert node_id new_event cache) new_event

run_widget::Event->Widget a->Engine a->Engine a
run_widget event (Trigger {trigger}) engine=trigger event engine
run_widget event (Io_trigger {io_trigger}) engine=create_request (Io (io_trigger event)) engine