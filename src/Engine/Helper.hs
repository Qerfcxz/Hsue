{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Helper where

import Engine.Container
import Engine.Type
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified Error.Error as EE
import qualified Control.Monad as CM
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DFo
import qualified Data.Functor as DFu
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified Foreign.StablePtr as FSP

catch_false::IO FCT.CBool->IO ()
catch_false io=do
    value<-io
    CM.unless (FMU.toBool value) (EE.quick_error "catch_false" 0)

catch_zero::(Eq a,Num a)=>a->IO ()
catch_zero number=case number of
    0->EE.quick_error "catch_zero" 0
    _->return ()

catch_null::FP.Ptr a->IO ()
catch_null ptr=CM.when (ptr==FP.nullPtr) (EE.quick_error "catch_null" 0)

return_catch_null::IO (FP.Ptr a)->IO (FP.Ptr a)
return_catch_null io=do
    ptr<-io
    if ptr==FP.nullPtr then EE.quick_error "return_catch_null" 0 else return ptr

with_string::String->(FP.Ptr FCT.CChar->IO a)->IO a
with_string string=DBS.useAsCString (DTE.encodeUtf8 (DT.pack string))

push_event::Engine a b c d e->b->IO ()
push_event engine custom=do
    ptr<-FSP.newStablePtr custom
    FMA.allocaBytesAligned SDLI.sdl_event_size SDLI.sdl_event_alignment $ \sdl_event->do
        FMU.fillBytes sdl_event 0 SDLI.sdl_event_size
        FS.poke (FP.castPtr sdl_event) (engine.event_number+1)
        let new_sdl_event=FP.castPtr sdl_event
        SDLI.sdl_user_event_data1_poke new_sdl_event (FSP.castStablePtrToPtr ptr)
        value<-SDLF.sdl_push_event new_sdl_event
        CM.unless (FMU.toBool value) (EE.quick_error "push_event" 0)

pop_event::FP.Ptr ()->IO a
pop_event sdl_event=do
    ptr<-SDLI.sdl_user_event_data1_peek sdl_event
    let new_ptr=FSP.castPtrToStablePtr ptr
    custom<-FSP.deRefStablePtr new_ptr
    FSP.freeStablePtr new_ptr
    return custom

selector_action::(a->Widget b c d e f->Engine b c d e f->Engine b c d e f)->Selector a->Widget b c d e f->Engine b c d e f->Engine b c d e f
selector_action action this_selector this_widget engine=case this_selector of
    None_selector->engine
    Combine_selector {combine_selector}->DFo.foldl' (\this_engine selector->selector_action action selector this_widget this_engine) engine combine_selector
    Self_selector {value}->action value this_widget engine
    All_selector {value,maybe_value}->all_selector_action (action value) this_widget (selector_action_a maybe_value action this_widget engine)
    Abstain_selector {value,maybe_value}->abstain_selector_action (action value) this_widget (selector_action_a maybe_value action this_widget engine)
    Trigger_selector {value,maybe_value}->trigger_selector_action (action value) this_widget (selector_action_a maybe_value action this_widget engine)
    Any_selector {strict,maybe_value,selector}->case this_widget of
        Group {group_widget}->DIM.foldl' (flip (selector_action action selector)) (selector_action_a maybe_value action this_widget engine) group_widget
        Widget_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget engine)
        Widget_io_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget engine)
        Widget_mix_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget engine)
        Coroutine {coroutine_state}->DIM.foldl' (\this_engine single_coroutine_state->selector_action action selector single_coroutine_state.widget this_engine) (selector_action_a maybe_value action this_widget engine) coroutine_state
        _->if strict then EE.quick_error "selector_action" 0 else selector_action_a maybe_value action this_widget engine
    Default_selector {strict,maybe_value,selector}->case this_widget of
        Group {index,group_widget}->selector_action action selector (intmap_lookup index group_widget) (selector_action_a maybe_value action this_widget engine)
        Widget_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget engine)
        Widget_io_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget engine)
        Widget_mix_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget engine)
        Coroutine {index,coroutine_state}->selector_action action selector (intmap_lookup index coroutine_state).widget (selector_action_a maybe_value action this_widget engine)
        _->if strict then EE.quick_error "selector_action" 1 else selector_action_a maybe_value action this_widget engine
    Group_any_selector {maybe_value,selector}->case this_widget of
        Group {group_widget}->DIM.foldl' (flip (selector_action action selector)) (selector_action_a maybe_value action this_widget engine) group_widget
        _->EE.quick_error "selector_action" 2
    Group_selector {maybe_value,group_selector}->case this_widget of
        Group {group_widget}->DIM.foldlWithKey' (\this_engine index selector->selector_action action selector (intmap_lookup index group_widget) this_engine) (selector_action_a maybe_value action this_widget engine) group_selector
        _->EE.quick_error "selector_action" 3
    Widget_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget engine)
        _->EE.quick_error "selector_action" 4
    Widget_io_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_io_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget engine)
        _->EE.quick_error "selector_action" 5
    Widget_mix_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_mix_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget engine)
        _->EE.quick_error "selector_action" 6
    Coroutine_any_selector {maybe_value,selector}->case this_widget of
        Coroutine {coroutine_state}->DIM.foldl' (\this_engine single_coroutine_state->selector_action action selector single_coroutine_state.widget this_engine) (selector_action_a maybe_value action this_widget engine) coroutine_state
        _->EE.quick_error "selector_action" 7
    Coroutine_selector {maybe_value,coroutine_selector}->case this_widget of
        Coroutine {coroutine_state}->DIM.foldlWithKey' (\this_engine index selector->selector_action action selector (intmap_lookup index coroutine_state).widget this_engine) (selector_action_a maybe_value action this_widget engine) coroutine_selector
        _->EE.quick_error "selector_action" 8

selector_action_a::Maybe a->(a->Widget b c d e f->Engine b c d e f->Engine b c d e f)->Widget b c d e f->Engine b c d e f->Engine b c d e f
selector_action_a maybe_value action widget engine=case maybe_value of
    Nothing->engine
    Just value->action value widget engine

all_selector_action::(Widget a b c d e->Engine a b c d e->Engine a b c d e)->Widget a b c d e->Engine a b c d e->Engine a b c d e
all_selector_action action this_widget engine=case this_widget of
    Group {group_widget}->DIM.foldl' (flip (all_selector_action action)) engine group_widget
    Widget_trigger {widget}->all_selector_action action widget engine
    Widget_io_trigger {widget}->all_selector_action action widget engine
    Widget_mix_trigger {widget}->all_selector_action action widget engine
    Coroutine {coroutine_state}->DIM.foldl' (\this_engine single_coroutine_state->all_selector_action action single_coroutine_state.widget this_engine) engine coroutine_state
    _->action this_widget engine

abstain_selector_action::(Widget a b c d e->Engine a b c d e->Engine a b c d e)->Widget a b c d e->Engine a b c d e->Engine a b c d e
abstain_selector_action action this_widget engine=case this_widget of
    Group {index,group_widget}->abstain_selector_action action (intmap_lookup index group_widget) engine
    Widget_trigger {widget}->abstain_selector_action action widget engine
    Widget_io_trigger {widget}->abstain_selector_action action widget engine
    Widget_mix_trigger {widget}->abstain_selector_action action widget engine
    Coroutine {index,coroutine_state}->abstain_selector_action action (intmap_lookup index coroutine_state).widget engine
    _->action this_widget engine

trigger_selector_action::(Widget a b c d e->Engine a b c d e->Engine a b c d e)->Widget a b c d e->Engine a b c d e->Engine a b c d e
trigger_selector_action action this_widget engine=case this_widget of
    Group {index,group_widget}->trigger_selector_action action (intmap_lookup index group_widget) engine
    Coroutine {index,coroutine_state}->trigger_selector_action action (intmap_lookup index coroutine_state).widget engine
    _->action this_widget engine

selector_monad_action::Monad g=>(a->Widget b c d e f->Engine b c d e f->g (Engine b c d e f))->Selector a->Widget b c d e f->Engine b c d e f->g (Engine b c d e f)
selector_monad_action action this_selector this_widget engine=case this_selector of
    None_selector->return engine
    Combine_selector {combine_selector}->DFo.foldlM (\this_engine selector->selector_monad_action action selector this_widget this_engine) engine combine_selector
    Self_selector {value}->action value this_widget engine
    All_selector {value,maybe_value}->do
        new_engine<-self_selector_applicative_action maybe_value action this_widget engine
        all_selector_monad_action (action value) this_widget new_engine
    Abstain_selector {value,maybe_value}->do
        new_engine<-self_selector_applicative_action maybe_value action this_widget engine
        abstain_selector_lifted_action (action value) this_widget new_engine
    Trigger_selector {value,maybe_value}->do
        new_engine<-self_selector_applicative_action maybe_value action this_widget engine
        trigger_selector_lifted_action (action value) this_widget new_engine
    Any_selector {strict,maybe_value,selector}->case this_widget of
        Group {group_widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            DFo.foldlM (flip (selector_monad_action action selector)) new_engine group_widget
        Widget_trigger {widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector widget new_engine
        Widget_io_trigger {widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector widget new_engine
        Widget_mix_trigger {widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector widget new_engine
        Coroutine {coroutine_state}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            DFo.foldlM (\this_engine single_coroutine_state->selector_monad_action action selector single_coroutine_state.widget this_engine) new_engine coroutine_state
        _->if strict then EE.quick_error "selector_monad_action" 0 else self_selector_applicative_action maybe_value action this_widget engine
    Default_selector {strict,maybe_value,selector}->case this_widget of
        Group {index,group_widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector (intmap_lookup index group_widget) new_engine
        Widget_trigger {widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector widget new_engine
        Widget_io_trigger {widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector widget new_engine
        Widget_mix_trigger {widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector widget new_engine
        Coroutine {index,coroutine_state}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector (intmap_lookup index coroutine_state).widget new_engine
        _->if strict then EE.quick_error "selector_monad_action" 1 else self_selector_applicative_action maybe_value action this_widget engine
    Group_any_selector {maybe_value,selector}->case this_widget of
        Group {group_widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            DFo.foldlM (flip (selector_monad_action action selector)) new_engine group_widget
        _->EE.quick_error "selector_monad_action" 2
    Group_selector {maybe_value,group_selector}->case this_widget of
        Group {group_widget}->do
            DIM.foldlWithKey' (\this_engine index selector->this_engine>>=selector_monad_action action selector (intmap_lookup index group_widget)) (self_selector_applicative_action maybe_value action this_widget engine) group_selector
        _->EE.quick_error "selector_monad_action" 3
    Widget_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_trigger {widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector widget new_engine
        _->EE.quick_error "selector_monad_action" 4
    Widget_io_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_io_trigger {widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector widget new_engine
        _->EE.quick_error "selector_monad_action" 5
    Widget_mix_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_mix_trigger {widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            selector_monad_action action selector widget new_engine
        _->EE.quick_error "selector_monad_action" 6
    Coroutine_any_selector {maybe_value,selector}->case this_widget of
        Coroutine {coroutine_state}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            DFo.foldlM (\this_engine single_coroutine_state->selector_monad_action action selector single_coroutine_state.widget this_engine) new_engine coroutine_state
        _->EE.quick_error "selector_monad_action" 7
    Coroutine_selector {maybe_value,coroutine_selector}->case this_widget of
        Coroutine {coroutine_state}->DIM.foldlWithKey' (\this_engine index selector->this_engine>>=selector_monad_action action selector (intmap_lookup index coroutine_state).widget) (self_selector_applicative_action maybe_value action this_widget engine) coroutine_selector
        _->EE.quick_error "selector_monad_action" 8

self_selector_applicative_action::Applicative g=>Maybe a->(a->Widget b c d e f->Engine b c d e f->g (Engine b c d e f))->Widget b c d e f->Engine b c d e f->g (Engine b c d e f)
self_selector_applicative_action maybe_value action widget engine=case maybe_value of
    Nothing->pure engine
    Just value->action value widget engine

all_selector_monad_action::Monad f=>(Widget a b c d e->Engine a b c d e->f (Engine a b c d e))->Widget a b c d e->Engine a b c d e->f (Engine a b c d e)
all_selector_monad_action action this_widget engine=case this_widget of
    Group {group_widget}->DFo.foldlM (flip (all_selector_monad_action action)) engine group_widget
    Widget_trigger {widget}->all_selector_monad_action action widget engine
    Widget_io_trigger {widget}->all_selector_monad_action action widget engine
    Widget_mix_trigger {widget}->all_selector_monad_action action widget engine
    Coroutine {coroutine_state}->DFo.foldlM (\this_engine single_coroutine_state->all_selector_monad_action action single_coroutine_state.widget this_engine) engine coroutine_state
    _->action this_widget engine

abstain_selector_lifted_action::(Widget a b c d e->Engine a b c d e->f (Engine a b c d e))->Widget a b c d e->Engine a b c d e->f (Engine a b c d e)
abstain_selector_lifted_action action this_widget engine=case this_widget of
    Group {index,group_widget}->abstain_selector_lifted_action action (intmap_lookup index group_widget) engine
    Widget_trigger {widget}->abstain_selector_lifted_action action widget engine
    Widget_io_trigger {widget}->abstain_selector_lifted_action action widget engine
    Widget_mix_trigger {widget}->abstain_selector_lifted_action action widget engine
    Coroutine {index,coroutine_state}->abstain_selector_lifted_action action (intmap_lookup index coroutine_state).widget engine
    _->action this_widget engine

trigger_selector_lifted_action::(Widget a b c d e->Engine a b c d e->f (Engine a b c d e))->Widget a b c d e->Engine a b c d e->f (Engine a b c d e)
trigger_selector_lifted_action action this_widget engine=case this_widget of
    Group {index,group_widget}->trigger_selector_lifted_action action (intmap_lookup index group_widget) engine
    Coroutine {index,coroutine_state}->trigger_selector_lifted_action action (intmap_lookup index coroutine_state).widget engine
    _->action this_widget engine

selector_update::(a->Widget b c d e f->Widget b c d e f)->Selector a->Widget b c d e f->Widget b c d e f
selector_update update this_selector this_widget=case this_selector of
    None_selector->this_widget
    Combine_selector {combine_selector}->DFo.foldl' (flip (selector_update update)) this_widget combine_selector
    Self_selector {value}->update value this_widget
    All_selector {value,maybe_value}->selector_update_a maybe_value update (all_selector_update (update value) this_widget)
    Abstain_selector {value,maybe_value}->selector_update_a maybe_value update (abstain_selector_update (update value) this_widget)
    Trigger_selector {value,maybe_value}->selector_update_a maybe_value update (trigger_selector_update (update value) this_widget)
    Any_selector {strict,maybe_value,selector}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=fmap (selector_update update selector) group_widget})
        Widget_trigger {next,widget_trigger,widget}->selector_update_a maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=selector_update update selector widget})
        Widget_io_trigger {next,widget_io_trigger,widget}->selector_update_a maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=selector_update update selector widget})
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->selector_update_a maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=selector_update update selector widget})
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=fmap (update_coroutine_state (selector_update update selector)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->if strict then EE.quick_error "selector_update" 0 else selector_update_a maybe_value update this_widget
    Default_selector {strict,maybe_value,selector}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=intmap_update index (selector_update update selector) group_widget})
        Widget_trigger {next,widget_trigger,widget}->selector_update_a maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=selector_update update selector widget})
        Widget_io_trigger {next,widget_io_trigger,widget}->selector_update_a maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=selector_update update selector widget})
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->selector_update_a maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=selector_update update selector widget})
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=intmap_update index (update_coroutine_state (selector_update update selector)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->if strict then EE.quick_error "selector_update" 1 else selector_update_a maybe_value update this_widget
    Group_any_selector {maybe_value,selector}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=fmap (selector_update update selector) group_widget})
        _->EE.quick_error "selector_update" 2
    Group_selector {maybe_value,group_selector}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=DIM.foldlWithKey' (\this_group_widget this_index selector->intmap_update this_index (selector_update update selector) this_group_widget) group_widget group_selector})
        _->EE.quick_error "selector_update" 3
    Widget_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_trigger {next,widget_trigger,widget}->selector_update_a maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=selector_update update selector widget})
        _->EE.quick_error "selector_update" 4
    Widget_io_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_io_trigger {next,widget_io_trigger,widget}->selector_update_a maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=selector_update update selector widget})
        _->EE.quick_error "selector_update" 5
    Widget_mix_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->selector_update_a maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=selector_update update selector widget})
        _->EE.quick_error "selector_update" 6
    Coroutine_any_selector {maybe_value,selector}->case this_widget of
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=fmap (update_coroutine_state (selector_update update selector)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->EE.quick_error "selector_update" 7
    Coroutine_selector {maybe_value,coroutine_selector}->case this_widget of
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=DIM.foldlWithKey' (\this_coroutine_state this_index selector->intmap_update this_index (update_coroutine_state (selector_update update selector)) this_coroutine_state) coroutine_state coroutine_selector,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->EE.quick_error "selector_update" 8

selector_update_a::Maybe a->(a->Widget b c d e f->Widget b c d e f)->Widget b c d e f->Widget b c d e f
selector_update_a maybe_value update widget=case maybe_value of
    Nothing->widget
    Just value->update value widget

all_selector_update::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
all_selector_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=fmap (all_selector_update update) group_widget}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=all_selector_update update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=all_selector_update update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=all_selector_update update widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=fmap (update_coroutine_state (all_selector_update update)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

abstain_selector_update::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
abstain_selector_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=intmap_update index (abstain_selector_update update) group_widget}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=abstain_selector_update update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=abstain_selector_update update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=abstain_selector_update update widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=intmap_update index (update_coroutine_state (abstain_selector_update update)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

trigger_selector_update::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
trigger_selector_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=intmap_update index (trigger_selector_update update) group_widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=intmap_update index (update_coroutine_state (trigger_selector_update update)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

selector_monad_update::Monad g=>(a->Widget b c d e f->g (Widget b c d e f))->Selector a->Widget b c d e f->g (Widget b c d e f)
selector_monad_update update this_selector this_widget=case this_selector of
    None_selector->return this_widget
    Combine_selector {combine_selector}->DFo.foldlM (flip (selector_monad_update update)) this_widget combine_selector
    Self_selector {value}->update value this_widget
    All_selector {value,maybe_value}->do
        new_widget<-all_selector_applicative_update (update value) this_widget
        self_selector_applicative_update maybe_value update new_widget
    Abstain_selector {value,maybe_value}->do
        new_widget<-abstain_selector_functor_update (update value) this_widget
        self_selector_applicative_update maybe_value update new_widget
    Trigger_selector {value,maybe_value}->do
        new_widget<-trigger_selector_functor_update (update value) this_widget
        self_selector_applicative_update maybe_value update new_widget
    Any_selector {strict,maybe_value,selector}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->do
            new_group_widget<-traverse (selector_monad_update update selector) group_widget
            self_selector_applicative_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=new_group_widget})
        Widget_trigger {next,widget_trigger,widget}->do
            new_widget<-selector_monad_update update selector widget
            self_selector_applicative_update maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=new_widget})
        Widget_io_trigger {next,widget_io_trigger,widget}->do
            new_widget<-selector_monad_update update selector widget
            self_selector_applicative_update maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=new_widget})
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->do
            new_widget<-selector_monad_update update selector widget
            self_selector_applicative_update maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=new_widget})
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->do
            new_coroutine_state<-traverse (functor_update_coroutine_state (selector_monad_update update selector)) coroutine_state
            self_selector_applicative_update maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->if strict then EE.quick_error "selector_monad_update" 0 else self_selector_applicative_update maybe_value update this_widget
    Default_selector {strict,maybe_value,selector}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->do
            new_group_widget<-intmap_functor_update index (selector_monad_update update selector) group_widget
            self_selector_applicative_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=new_group_widget})
        Widget_trigger {next,widget_trigger,widget}->do
            new_widget<-selector_monad_update update selector widget
            self_selector_applicative_update maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=new_widget})
        Widget_io_trigger {next,widget_io_trigger,widget}->do
            new_widget<-selector_monad_update update selector widget
            self_selector_applicative_update maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=new_widget})
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->do
            new_widget<-selector_monad_update update selector widget
            self_selector_applicative_update maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=new_widget})
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->do
            new_coroutine_state<-intmap_functor_update index (functor_update_coroutine_state (selector_monad_update update selector)) coroutine_state
            self_selector_applicative_update maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->if strict then EE.quick_error "selector_monad_update" 1 else self_selector_applicative_update maybe_value update this_widget
    Group_any_selector {maybe_value,selector}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->do
            new_group_widget<-traverse (selector_monad_update update selector) group_widget
            self_selector_applicative_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=new_group_widget})
        _->EE.quick_error "selector_monad_update" 2
    Group_selector {maybe_value,group_selector}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->do
            new_group_widget<-intmap_applicative_action (selector_monad_update update) group_selector group_widget
            self_selector_applicative_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=new_group_widget})
        _->EE.quick_error "selector_monad_update" 3
    Widget_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_trigger {next,widget_trigger,widget}->do
            new_widget<-selector_monad_update update selector widget
            self_selector_applicative_update maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=new_widget})
        _->EE.quick_error "selector_monad_update" 4
    Widget_io_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_io_trigger {next,widget_io_trigger,widget}->do
            new_widget<-selector_monad_update update selector widget
            self_selector_applicative_update maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=new_widget})
        _->EE.quick_error "selector_monad_update" 5
    Widget_mix_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->do
            new_widget<-selector_monad_update update selector widget
            self_selector_applicative_update maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=new_widget})
        _->EE.quick_error "selector_monad_update" 6
    Coroutine_any_selector {maybe_value,selector}->case this_widget of
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->do
            new_coroutine_state<-traverse (functor_update_coroutine_state (selector_monad_update update selector)) coroutine_state
            self_selector_applicative_update maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->EE.quick_error "selector_monad_update" 7
    Coroutine_selector {maybe_value,coroutine_selector}->case this_widget of
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->do
            new_coroutine_state<-intmap_applicative_action (functor_update_coroutine_state . selector_monad_update update) coroutine_selector coroutine_state
            self_selector_applicative_update maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->EE.quick_error "selector_monad_update" 8

self_selector_applicative_update::Applicative g=>Maybe a->(a->Widget b c d e f->g (Widget b c d e f))->Widget b c d e f->g (Widget b c d e f)
self_selector_applicative_update maybe_value update widget=case maybe_value of
    Nothing->pure widget
    Just value->update value widget

all_selector_applicative_update::Applicative f=>(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
all_selector_applicative_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (traverse (all_selector_applicative_update update) group_widget)
    Widget_trigger {next,widget_trigger,widget}->fmap (\this_this_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=this_this_widget}) (all_selector_applicative_update update widget)
    Widget_io_trigger {next,widget_io_trigger,widget}->fmap (\this_this_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=this_this_widget}) (all_selector_applicative_update update widget)
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->fmap (\this_this_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=this_this_widget}) (all_selector_applicative_update update widget)
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (traverse (functor_update_coroutine_state (all_selector_applicative_update update)) coroutine_state)
    _->update this_widget

abstain_selector_functor_update::Functor f=>(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
abstain_selector_functor_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (abstain_selector_functor_update update) group_widget)
    Widget_trigger {next,widget_trigger,widget}->fmap (\this_this_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=this_this_widget}) (abstain_selector_functor_update update widget)
    Widget_io_trigger {next,widget_io_trigger,widget}->fmap (\this_this_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=this_this_widget}) (abstain_selector_functor_update update widget)
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->fmap (\this_this_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=this_this_widget}) (abstain_selector_functor_update update widget)
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (abstain_selector_functor_update update)) coroutine_state)
    _->update this_widget

trigger_selector_functor_update::Functor f=>(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
trigger_selector_functor_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (trigger_selector_functor_update update) group_widget)
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (trigger_selector_functor_update update)) coroutine_state)
    _->update this_widget

widget_lookup::Widget a b c d e->Widget a b c d e
widget_lookup this_widget=case this_widget of
    Group {index,group_widget}->widget_lookup (intmap_lookup index group_widget)
    Widget_trigger {widget}->widget_lookup widget
    Widget_io_trigger {widget}->widget_lookup widget
    Widget_mix_trigger {widget}->widget_lookup widget
    Coroutine {index,coroutine_state}->widget_lookup (intmap_lookup index coroutine_state).widget
    _->this_widget

update_coroutine_state::(Widget a b c d e->Widget a b c d e)->Coroutine_state a b c d e->Coroutine_state a b c d e
update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->Coroutine_state {widget=update widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}

functor_update_coroutine_state::Functor f=>(Widget a b c d e->f (Widget a b c d e))->Coroutine_state a b c d e->f (Coroutine_state a b c d e)
functor_update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->fmap (\this_widget->Coroutine_state {widget=this_widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}) (update widget)

seq_poke_array::FS.Storable a=>Int->DS.Seq a->FP.Ptr a->IO ()
seq_poke_array size value ptr=DFu.void (DFo.foldlM (flip (seq_poke_array_a size)) ptr value)

seq_poke_array_a::FS.Storable a=>Int->a->FP.Ptr a->IO (FP.Ptr a)
seq_poke_array_a size value ptr=do
    FS.poke ptr value
    return (FP.plusPtr ptr size)

triple_reverse::(a,b,c)->(c,b,a)
triple_reverse (a,b,c)=(c,b,a)

identity_matrix::Matrix
identity_matrix=Matrix {x=0,y=0,x_x=1,x_y=0,y_x=0,y_y=1}

to_extended::FCT.CFloat->Extended
to_extended number=Finite {number=number}

from_extended::Extended->FCT.CFloat
from_extended extended=case extended of
    Negative_infinity->0
    Finite {number}->number
    Positive_infinity->0

get_clipboard_text::IO String
get_clipboard_text=do
    ptr<-SDLF.sdl_get_clipboard_text
    catch_null ptr
    string<-FCS.peekCString ptr
    SDLF.sdl_free (FP.castPtr ptr)
    return string

has_clipboard_text::IO Bool
has_clipboard_text=fmap FMU.toBool SDLF.sdl_has_clipboard_text

set_clipboard_text::String->IO Bool
set_clipboard_text string=with_string string $ \ptr->do
    value<-SDLF.sdl_set_clipboard_text ptr
    return (FMU.toBool value)

quick_create_engine::(a->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Projection_strategy)->FCT.CInt->Int->Int->Int->Int->Int->Int->Maybe DW.Word64->DW.Word64->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->IO (Engine a b c d e))->a->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Projection_strategy)->FCT.CInt->Int->Int->Int->Maybe DW.Word64->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->IO (Engine a b c d e)
quick_create_engine create_engine state main_id projection_strategy picture_size vertex_size index_size parameter_size maybe_interval padding width height font_size pixel_range=case maybe_interval of
    Nothing->create_engine state main_id projection_strategy (picture_size*mebibyte) (vertex_size*mebibyte) (index_size*mebibyte) (parameter_size*mebibyte) 0 0 0 Nothing 0 padding width height font_size pixel_range
    Just interval->create_engine state main_id projection_strategy (picture_size*mebibyte) (vertex_size*mebibyte) (index_size*mebibyte) (parameter_size*mebibyte) 0 0 0 (Just (div nanosecond interval)) 0 padding width height font_size pixel_range

mebibyte::Num a=>a
mebibyte=1048576

nanosecond::Num a=>a
nanosecond=1000000000

millisecond::Num a=>a
millisecond=1000000