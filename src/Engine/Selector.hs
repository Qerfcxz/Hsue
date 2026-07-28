{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Selector where

import Engine.Container
import Engine.Type
import qualified Error.Error as EE
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM

selector_action::(a->Widget b c d e f->Engine b c d e f->Engine b c d e f)->Selector a->Widget b c d e f->Engine b c d e f->Engine b c d e f
selector_action action this_selector this_widget engine=case this_selector of
    None_selector->engine
    Combine_selector {combine_selector}->DF.foldl' (\this_engine single_selector->selector_action action single_selector this_widget this_engine) engine combine_selector
    Self_selector {value}->action value this_widget engine
    All_selector {maybe_value,value}->all_selector_action (action value) this_widget (self_selector_action maybe_value action this_widget engine)
    Trigger_selector {maybe_value,value,bounded}->trigger_selector_action bounded (action value) this_widget (self_selector_action maybe_value action this_widget engine)
    Abstain_selector {maybe_value,value,bounded}->abstain_selector_action bounded (action value) this_widget (self_selector_action maybe_value action this_widget engine)
    Default_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {index,group_widget}->let backup=self_selector_action maybe_value action this_widget engine in if bounded then selector_action action selector (intmap_lookup index group_widget) backup else maybe backup (\widget->selector_action action selector widget backup) (DIM.lookup index group_widget)
        Vector {index,vector_widget}->if bounded then selector_action action selector (vector_widget DV.! index) (self_selector_action maybe_value action this_widget engine) else maybe (self_selector_action maybe_value action this_widget engine) (\widget->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)) (vector_widget DV.!? index)
        Widget_trigger {widget}->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)
        Widget_io_trigger {widget}->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)
        Widget_mix_trigger {widget}->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)
        Coroutine {index,coroutine_state}->let backup=self_selector_action maybe_value action this_widget engine in if bounded then selector_action action selector (intmap_lookup index coroutine_state).widget backup else maybe backup (\single_coroutine_state->selector_action action selector single_coroutine_state.widget backup) (DIM.lookup index coroutine_state)
        _->if strict then EE.quick_error "selector_action" 0 else self_selector_action maybe_value action this_widget engine
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {group_widget}->DIM.foldl' (flip (selector_action action selector)) (self_selector_action maybe_value action this_widget engine) group_widget
        Vector {vector_widget}->DF.foldl' (flip (selector_action action selector)) (self_selector_action maybe_value action this_widget engine) vector_widget
        Widget_trigger {widget}->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)
        Widget_io_trigger {widget}->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)
        Widget_mix_trigger {widget}->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)
        Coroutine {coroutine_state}->DIM.foldl' (\this_engine single_coroutine_state->selector_action action selector single_coroutine_state.widget this_engine) (self_selector_action maybe_value action this_widget engine) coroutine_state
        _->if strict then EE.quick_error "selector_action" 1 else self_selector_action maybe_value action this_widget engine
    Group_selector {maybe_value,group_selector,bounded}->case this_widget of
        Group {group_widget}->DIM.foldlWithKey' (\this_engine index single_selector->if bounded then selector_action action single_selector (intmap_lookup index group_widget) this_engine else maybe this_engine (\single_widget->selector_action action single_selector single_widget this_engine) (DIM.lookup index group_widget)) (self_selector_action maybe_value action this_widget engine) group_selector
        _->EE.quick_error "selector_action" 2
    Vector_selector {maybe_value,vector_selector,bounded}->case this_widget of
        Vector {vector_widget}->DIM.foldlWithKey' (\this_engine index single_selector->if bounded then selector_action action single_selector (vector_widget DV.! index) this_engine else maybe this_engine (\single_widget->selector_action action single_selector single_widget this_engine) (vector_widget DV.!? index)) (self_selector_action maybe_value action this_widget engine) vector_selector
        _->EE.quick_error "selector_action" 3
    Widget_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_trigger {widget}->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)
        _->EE.quick_error "selector_action" 4
    Widget_io_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_io_trigger {widget}->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)
        _->EE.quick_error "selector_action" 5
    Widget_mix_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_mix_trigger {widget}->selector_action action selector widget (self_selector_action maybe_value action this_widget engine)
        _->EE.quick_error "selector_action" 6
    Coroutine_selector {maybe_value,coroutine_selector,bounded}->case this_widget of
        Coroutine {coroutine_state}->DIM.foldlWithKey' (\this_engine index single_selector->if bounded then selector_action action single_selector (intmap_lookup index coroutine_state).widget this_engine else maybe this_engine (\single_coroutine_state->selector_action action single_selector single_coroutine_state.widget this_engine) (DIM.lookup index coroutine_state)) (self_selector_action maybe_value action this_widget engine) coroutine_selector
        _->EE.quick_error "selector_action" 7

self_selector_action::Maybe a->(a->Widget b c d e f->Engine b c d e f->Engine b c d e f)->Widget b c d e f->Engine b c d e f->Engine b c d e f
self_selector_action maybe_value action widget engine=case maybe_value of
    Nothing->engine
    Just value->action value widget engine

all_selector_action::(Widget a b c d e->Engine a b c d e->Engine a b c d e)->Widget a b c d e->Engine a b c d e->Engine a b c d e
all_selector_action action this_widget engine=case this_widget of
    Group {group_widget}->DIM.foldl' (flip (all_selector_action action)) engine group_widget
    Vector {vector_widget}->DF.foldl' (flip (all_selector_action action)) engine vector_widget
    Widget_trigger {widget}->all_selector_action action widget engine
    Widget_io_trigger {widget}->all_selector_action action widget engine
    Widget_mix_trigger {widget}->all_selector_action action widget engine
    Coroutine {coroutine_state}->DIM.foldl' (\this_engine single_coroutine_state->all_selector_action action single_coroutine_state.widget this_engine) engine coroutine_state
    _->action this_widget engine

trigger_selector_action::Bool->(Widget a b c d e->Engine a b c d e->Engine a b c d e)->Widget a b c d e->Engine a b c d e->Engine a b c d e
trigger_selector_action bounded action this_widget engine=case this_widget of
    Group {index,group_widget}->if bounded then trigger_selector_action bounded action (intmap_lookup index group_widget) engine else maybe engine (\widget->trigger_selector_action bounded action widget engine) (DIM.lookup index group_widget)
    Vector {index,vector_widget}->if bounded then trigger_selector_action bounded action (vector_widget DV.! index) engine else maybe engine (\widget->trigger_selector_action bounded action widget engine) (vector_widget DV.!? index)
    Coroutine {index,coroutine_state}->if bounded then trigger_selector_action bounded action (intmap_lookup index coroutine_state).widget engine else maybe engine (\single_coroutine_state->trigger_selector_action bounded action single_coroutine_state.widget engine) (DIM.lookup index coroutine_state)
    _->action this_widget engine

abstain_selector_action::Bool->(Widget a b c d e->Engine a b c d e->Engine a b c d e)->Widget a b c d e->Engine a b c d e->Engine a b c d e
abstain_selector_action bounded action this_widget engine=case this_widget of
    Group {index,group_widget}->if bounded then abstain_selector_action bounded action (intmap_lookup index group_widget) engine else maybe engine (\widget->abstain_selector_action bounded action widget engine) (DIM.lookup index group_widget)
    Vector {index,vector_widget}->if bounded then abstain_selector_action bounded action (vector_widget DV.! index) engine else maybe engine (\widget->abstain_selector_action bounded action widget engine) (vector_widget DV.!? index)
    Widget_trigger {widget}->abstain_selector_action bounded action widget engine
    Widget_io_trigger {widget}->abstain_selector_action bounded action widget engine
    Widget_mix_trigger {widget}->abstain_selector_action bounded action widget engine
    Coroutine {index,coroutine_state}->if bounded then abstain_selector_action bounded action (intmap_lookup index coroutine_state).widget engine else maybe engine (\single_coroutine_state->abstain_selector_action bounded action single_coroutine_state.widget engine) (DIM.lookup index coroutine_state)
    _->action this_widget engine

selector_monad_action::Monad g=>(a->Widget b c d e f->Engine b c d e f->g (Engine b c d e f))->Selector a->Widget b c d e f->Engine b c d e f->g (Engine b c d e f)
selector_monad_action action this_selector this_widget engine=case this_selector of
    None_selector->return engine
    Combine_selector {combine_selector}->DF.foldlM (\this_engine single_selector->selector_monad_action action single_selector this_widget this_engine) engine combine_selector
    Self_selector {value}->action value this_widget engine
    All_selector {maybe_value,value}->do
        new_engine<-self_selector_applicative_action maybe_value action this_widget engine
        all_selector_monad_action (action value) this_widget new_engine
    Trigger_selector {maybe_value,value,bounded}->do
        new_engine<-self_selector_applicative_action maybe_value action this_widget engine
        trigger_selector_monad_action bounded (action value) this_widget new_engine
    Abstain_selector {maybe_value,value,bounded}->do
        new_engine<-self_selector_applicative_action maybe_value action this_widget engine
        abstain_selector_monad_action bounded (action value) this_widget new_engine
    Default_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {index,group_widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            if bounded then selector_monad_action action selector (intmap_lookup index group_widget) new_engine else maybe (return new_engine) (\widget->selector_monad_action action selector widget new_engine) (DIM.lookup index group_widget)
        Vector {index,vector_widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            if bounded then selector_monad_action action selector (vector_widget DV.! index) new_engine else maybe (return new_engine) (\widget->selector_monad_action action selector widget new_engine) (vector_widget DV.!? index)
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
            if bounded then selector_monad_action action selector (intmap_lookup index coroutine_state).widget new_engine else maybe (return new_engine) (\single_coroutine_state->selector_monad_action action selector single_coroutine_state.widget new_engine) (DIM.lookup index coroutine_state)
        _->if strict then EE.quick_error "selector_monad_action" 0 else self_selector_applicative_action maybe_value action this_widget engine
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {group_widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            DF.foldlM (flip (selector_monad_action action selector)) new_engine group_widget
        Vector {vector_widget}->do
            new_engine<-self_selector_applicative_action maybe_value action this_widget engine
            DF.foldlM (flip (selector_monad_action action selector)) new_engine vector_widget
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
            DF.foldlM (\this_engine single_coroutine_state->selector_monad_action action selector single_coroutine_state.widget this_engine) new_engine coroutine_state
        _->if strict then EE.quick_error "selector_monad_action" 1 else self_selector_applicative_action maybe_value action this_widget engine
    Group_selector {maybe_value,group_selector,bounded}->case this_widget of
        Group {group_widget}->DIM.foldlWithKey' (\this_engine index single_selector->if bounded then this_engine>>=selector_monad_action action single_selector (intmap_lookup index group_widget) else maybe this_engine (\single_widget->this_engine>>=selector_monad_action action single_selector single_widget) (DIM.lookup index group_widget)) (self_selector_applicative_action maybe_value action this_widget engine) group_selector
        _->EE.quick_error "selector_monad_action" 2
    Vector_selector {maybe_value,vector_selector,bounded}->case this_widget of
        Vector {vector_widget}->DIM.foldlWithKey' (\this_engine index single_selector->if bounded then this_engine>>=selector_monad_action action single_selector (vector_widget DV.! index) else maybe this_engine (\single_widget->this_engine>>=selector_monad_action action single_selector single_widget) (vector_widget DV.!? index)) (self_selector_applicative_action maybe_value action this_widget engine) vector_selector
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
    Coroutine_selector {maybe_value,coroutine_selector,bounded}->case this_widget of
        Coroutine {coroutine_state}->DIM.foldlWithKey' (\this_engine index single_selector->if bounded then this_engine>>=selector_monad_action action single_selector (intmap_lookup index coroutine_state).widget else maybe this_engine (\single_coroutine_state->this_engine>>=selector_monad_action action single_selector single_coroutine_state.widget) (DIM.lookup index coroutine_state)) (self_selector_applicative_action maybe_value action this_widget engine) coroutine_selector
        _->EE.quick_error "selector_monad_action" 7

self_selector_applicative_action::Applicative g=>Maybe a->(a->Widget b c d e f->Engine b c d e f->g (Engine b c d e f))->Widget b c d e f->Engine b c d e f->g (Engine b c d e f)
self_selector_applicative_action maybe_value action widget engine=case maybe_value of
    Nothing->pure engine
    Just value->action value widget engine

all_selector_monad_action::Monad f=>(Widget a b c d e->Engine a b c d e->f (Engine a b c d e))->Widget a b c d e->Engine a b c d e->f (Engine a b c d e)
all_selector_monad_action action this_widget engine=case this_widget of
    Group {group_widget}->DF.foldlM (flip (all_selector_monad_action action)) engine group_widget
    Vector {vector_widget}->DF.foldlM (flip (all_selector_monad_action action)) engine vector_widget
    Widget_trigger {widget}->all_selector_monad_action action widget engine
    Widget_io_trigger {widget}->all_selector_monad_action action widget engine
    Widget_mix_trigger {widget}->all_selector_monad_action action widget engine
    Coroutine {coroutine_state}->DF.foldlM (\this_engine single_coroutine_state->all_selector_monad_action action single_coroutine_state.widget this_engine) engine coroutine_state
    _->action this_widget engine

trigger_selector_monad_action::Monad f=>Bool->(Widget a b c d e->Engine a b c d e->f (Engine a b c d e))->Widget a b c d e->Engine a b c d e->f (Engine a b c d e)
trigger_selector_monad_action bounded action this_widget engine=case this_widget of
    Group {index,group_widget}->if bounded then trigger_selector_monad_action bounded action (intmap_lookup index group_widget) engine else maybe (return engine) (\widget->trigger_selector_monad_action bounded action widget engine) (DIM.lookup index group_widget)
    Vector {index,vector_widget}->if bounded then trigger_selector_monad_action bounded action (vector_widget DV.! index) engine else maybe (return engine) (\widget->trigger_selector_monad_action bounded action widget engine) (vector_widget DV.!? index)
    Coroutine {index,coroutine_state}->if bounded then trigger_selector_monad_action bounded action (intmap_lookup index coroutine_state).widget engine else maybe (return engine) (\single_coroutine_state->trigger_selector_monad_action bounded action single_coroutine_state.widget engine) (DIM.lookup index coroutine_state)
    _->action this_widget engine

abstain_selector_monad_action::Monad f=>Bool->(Widget a b c d e->Engine a b c d e->f (Engine a b c d e))->Widget a b c d e->Engine a b c d e->f (Engine a b c d e)
abstain_selector_monad_action bounded action this_widget engine=case this_widget of
    Group {index,group_widget}->if bounded then abstain_selector_monad_action bounded action (intmap_lookup index group_widget) engine else maybe (return engine) (\widget->abstain_selector_monad_action bounded action widget engine) (DIM.lookup index group_widget)
    Vector {index,vector_widget}->if bounded then abstain_selector_monad_action bounded action (vector_widget DV.! index) engine else maybe (return engine) (\widget->abstain_selector_monad_action bounded action widget engine) (vector_widget DV.!? index)
    Widget_trigger {widget}->abstain_selector_monad_action bounded action widget engine
    Widget_io_trigger {widget}->abstain_selector_monad_action bounded action widget engine
    Widget_mix_trigger {widget}->abstain_selector_monad_action bounded action widget engine
    Coroutine {index,coroutine_state}->if bounded then abstain_selector_monad_action bounded action (intmap_lookup index coroutine_state).widget engine else maybe (return engine) (\single_coroutine_state->abstain_selector_monad_action bounded action single_coroutine_state.widget engine) (DIM.lookup index coroutine_state)
    _->action this_widget engine

selector_update::(a->Widget b c d e f->Widget b c d e f)->Selector a->Widget b c d e f->Widget b c d e f
selector_update update this_selector this_widget=case this_selector of
    None_selector->this_widget
    Combine_selector {combine_selector}->DF.foldl' (flip (selector_update update)) this_widget combine_selector
    Self_selector {value}->update value this_widget
    All_selector {maybe_value,value}->self_selector_update maybe_value update (all_selector_update (update value) this_widget)
    Trigger_selector {maybe_value,value,bounded}->self_selector_update maybe_value update (trigger_selector_update bounded (update value) this_widget)
    Abstain_selector {maybe_value,value,bounded}->self_selector_update maybe_value update (abstain_selector_update bounded (update value) this_widget)
    Default_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->self_selector_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=(if bounded then intmap_update else intmap_update_safe) index (selector_update update selector) group_widget})
        Vector {index,size,vector_widget}->self_selector_update maybe_value update (Vector {index=index,size=size,vector_widget=if bounded then DV.modify (\this_vector_widget->DVM.write this_vector_widget index (selector_update update selector (vector_widget DV.! index))) vector_widget else maybe vector_widget (\widget->DV.modify (\this_vector_widget->DVM.write this_vector_widget index (selector_update update selector widget)) vector_widget) (vector_widget DV.!? index)})
        Widget_trigger {next,widget_trigger,widget}->self_selector_update maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=selector_update update selector widget})
        Widget_io_trigger {next,widget_io_trigger,widget}->self_selector_update maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=selector_update update selector widget})
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->self_selector_update maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=selector_update update selector widget})
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->self_selector_update maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=(if bounded then intmap_update else intmap_update_safe) index (update_coroutine_state (selector_update update selector)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->if strict then EE.quick_error "selector_update" 0 else self_selector_update maybe_value update this_widget
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->self_selector_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=fmap (selector_update update selector) group_widget})
        Vector {index,size,vector_widget}->self_selector_update maybe_value update (Vector {index=index,size=size,vector_widget=fmap (selector_update update selector) vector_widget})
        Widget_trigger {next,widget_trigger,widget}->self_selector_update maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=selector_update update selector widget})
        Widget_io_trigger {next,widget_io_trigger,widget}->self_selector_update maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=selector_update update selector widget})
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->self_selector_update maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=selector_update update selector widget})
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->self_selector_update maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=fmap (update_coroutine_state (selector_update update selector)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->if strict then EE.quick_error "selector_update" 1 else self_selector_update maybe_value update this_widget
    Group_selector {maybe_value,group_selector,bounded}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->self_selector_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=DIM.foldlWithKey' (\this_group_widget this_index single_selector->(if bounded then intmap_update else intmap_update_safe) this_index (selector_update update single_selector) this_group_widget) group_widget group_selector})
        _->EE.quick_error "selector_update" 2
    Vector_selector {maybe_value,vector_selector,bounded}->case this_widget of
        Vector {index,size,vector_widget}->self_selector_update maybe_value update (Vector {index=index,size=size,vector_widget=DV.modify (\this_vector_widget->CM.void (DIM.traverseWithKey (\this_index single_selector->if bounded then DVM.write this_vector_widget this_index (selector_update update single_selector (vector_widget DV.! this_index)) else maybe (return ()) (DVM.write this_vector_widget this_index . selector_update update single_selector) (vector_widget DV.!? this_index)) vector_selector)) vector_widget})
        _->EE.quick_error "selector_update" 3
    Widget_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_trigger {next,widget_trigger,widget}->self_selector_update maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=selector_update update selector widget})
        _->EE.quick_error "selector_update" 4
    Widget_io_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_io_trigger {next,widget_io_trigger,widget}->self_selector_update maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=selector_update update selector widget})
        _->EE.quick_error "selector_update" 5
    Widget_mix_trigger_selector {maybe_value,selector}->case this_widget of
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->self_selector_update maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=selector_update update selector widget})
        _->EE.quick_error "selector_update" 6
    Coroutine_selector {maybe_value,coroutine_selector,bounded}->case this_widget of
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->self_selector_update maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=DIM.foldlWithKey' (\this_coroutine_state this_index single_selector->(if bounded then intmap_update else intmap_update_safe) this_index (update_coroutine_state (selector_update update single_selector)) this_coroutine_state) coroutine_state coroutine_selector,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->EE.quick_error "selector_update" 7

self_selector_update::Maybe a->(a->Widget b c d e f->Widget b c d e f)->Widget b c d e f->Widget b c d e f
self_selector_update maybe_value update widget=case maybe_value of
    Nothing->widget
    Just value->update value widget

all_selector_update::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
all_selector_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=fmap (all_selector_update update) group_widget}
    Vector {index,size,vector_widget}->Vector {index=index,size=size,vector_widget=fmap (all_selector_update update) vector_widget}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=all_selector_update update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=all_selector_update update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=all_selector_update update widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=fmap (update_coroutine_state (all_selector_update update)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

trigger_selector_update::Bool->(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
trigger_selector_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=(if bounded then intmap_update else intmap_update_safe) index (trigger_selector_update bounded update) group_widget}
    Vector {index,size,vector_widget}->Vector {index=index,size=size,vector_widget=if bounded then DV.modify (\this_vector_widget->DVM.write this_vector_widget index (trigger_selector_update bounded update (vector_widget DV.! index))) vector_widget else maybe vector_widget (\widget->DV.modify (\this_vector_widget->DVM.write this_vector_widget index (trigger_selector_update bounded update widget)) vector_widget) (vector_widget DV.!? index)}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=(if bounded then intmap_update else intmap_update_safe) index (update_coroutine_state (trigger_selector_update bounded update)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

abstain_selector_update::Bool->(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
abstain_selector_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=(if bounded then intmap_update else intmap_update_safe) index (abstain_selector_update bounded update) group_widget}
    Vector {index,size,vector_widget}->Vector {index=index,size=size,vector_widget=if bounded then DV.modify (\this_vector_widget->DVM.write this_vector_widget index (abstain_selector_update bounded update (vector_widget DV.! index))) vector_widget else maybe vector_widget (\widget->DV.modify (\this_vector_widget->DVM.write this_vector_widget index (abstain_selector_update bounded update widget)) vector_widget) (vector_widget DV.!? index)}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=abstain_selector_update bounded update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=abstain_selector_update bounded update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=abstain_selector_update bounded update widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=(if bounded then intmap_update else intmap_update_safe) index (update_coroutine_state (abstain_selector_update bounded update)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

selector_monad_update::Monad g=>(a->Widget b c d e f->g (Widget b c d e f))->Selector a->Widget b c d e f->g (Widget b c d e f)
selector_monad_update update this_selector this_widget=case this_selector of
    None_selector->return this_widget
    Combine_selector {combine_selector}->DF.foldlM (flip (selector_monad_update update)) this_widget combine_selector
    Self_selector {value}->update value this_widget
    All_selector {maybe_value,value}->do
        new_widget<-all_selector_applicative_update (update value) this_widget
        self_selector_applicative_update maybe_value update new_widget
    Trigger_selector {maybe_value,value,bounded}->do
        new_widget<-trigger_selector_functor_update bounded (update value) this_widget
        self_selector_applicative_update maybe_value update new_widget
    Abstain_selector {maybe_value,value,bounded}->do
        new_widget<-abstain_selector_functor_update bounded (update value) this_widget
        self_selector_applicative_update maybe_value update new_widget
    Default_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->do
            new_group_widget<-if bounded then intmap_functor_update index (selector_monad_update update selector) group_widget else maybe (return group_widget) (\_->intmap_functor_update index (selector_monad_update update selector) group_widget) (DIM.lookup index group_widget)
            self_selector_applicative_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=new_group_widget})
        Vector {index,size,vector_widget}->if bounded
            then do
                new_widget<-selector_monad_update update selector (vector_widget DV.! index)
                self_selector_applicative_update maybe_value update (Vector {index=index,size=size,vector_widget=DV.modify (\this_vector_widget->DVM.write this_vector_widget index new_widget) vector_widget})
            else maybe (self_selector_applicative_update maybe_value update this_widget) (selector_monad_update update selector CM.>=> (\widget->self_selector_applicative_update maybe_value update (Vector {index=index,size=size,vector_widget=DV.modify (\this_vector_widget->DVM.write this_vector_widget index widget) vector_widget}))) (vector_widget DV.!? index)
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
            new_coroutine_state<-if bounded then intmap_functor_update index (functor_update_coroutine_state (selector_monad_update update selector)) coroutine_state else maybe (return coroutine_state) (\_->intmap_functor_update index (functor_update_coroutine_state (selector_monad_update update selector)) coroutine_state) (DIM.lookup index coroutine_state)
            self_selector_applicative_update maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->if strict then EE.quick_error "selector_monad_update" 0 else self_selector_applicative_update maybe_value update this_widget
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->do
            new_group_widget<-traverse (selector_monad_update update selector) group_widget
            self_selector_applicative_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=new_group_widget})
        Vector {index,size,vector_widget}->do
            new_vector_widget<-traverse (selector_monad_update update selector) vector_widget
            self_selector_applicative_update maybe_value update (Vector {index=index,size=size,vector_widget=new_vector_widget})
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
        _->if strict then EE.quick_error "selector_monad_update" 1 else self_selector_applicative_update maybe_value update this_widget
    Group_selector {maybe_value,group_selector,bounded}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->do
            new_group_widget<-(if bounded then intmap_applicative_action else intmap_applicative_action_safe) (selector_monad_update update) group_selector group_widget
            self_selector_applicative_update maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=new_group_widget})
        _->EE.quick_error "selector_monad_update" 2
    Vector_selector {maybe_value,vector_selector,bounded}->case this_widget of
        Vector {index,size,vector_widget}->do
            new_vector_widget<-DIM.foldlWithKey' (\action this_index single_selector->if bounded then action>>=(\this_vector_widget->selector_monad_update update single_selector (vector_widget DV.! this_index)>>=(\new_widget->return (DV.modify (\this_this_vector_widget->DVM.write this_this_vector_widget this_index new_widget) this_vector_widget))) else maybe action (\single_widget->action>>=(\this_vector_widget->selector_monad_update update single_selector single_widget>>=(\new_widget->return (DV.modify (\this_this_vector_widget->DVM.write this_this_vector_widget this_index new_widget) this_vector_widget)))) (vector_widget DV.!? this_index)) (return vector_widget) vector_selector
            self_selector_applicative_update maybe_value update (Vector {index=index,size=size,vector_widget=new_vector_widget})
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
    Coroutine_selector {maybe_value,coroutine_selector,bounded}->case this_widget of
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->do
            new_coroutine_state<-(if bounded then intmap_applicative_action else intmap_applicative_action_safe) (functor_update_coroutine_state . selector_monad_update update) coroutine_selector coroutine_state
            self_selector_applicative_update maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->EE.quick_error "selector_monad_update" 7

self_selector_applicative_update::Applicative g=>Maybe a->(a->Widget b c d e f->g (Widget b c d e f))->Widget b c d e f->g (Widget b c d e f)
self_selector_applicative_update maybe_value update widget=case maybe_value of
    Nothing->pure widget
    Just value->update value widget

all_selector_applicative_update::Applicative f=>(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
all_selector_applicative_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (traverse (all_selector_applicative_update update) group_widget)
    Vector {index,size,vector_widget}->fmap (\this_vector_widget->Vector {index=index,size=size,vector_widget=this_vector_widget}) (traverse (all_selector_applicative_update update) vector_widget)
    Widget_trigger {next,widget_trigger,widget}->fmap (\this_this_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=this_this_widget}) (all_selector_applicative_update update widget)
    Widget_io_trigger {next,widget_io_trigger,widget}->fmap (\this_this_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=this_this_widget}) (all_selector_applicative_update update widget)
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->fmap (\this_this_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=this_this_widget}) (all_selector_applicative_update update widget)
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (traverse (functor_update_coroutine_state (all_selector_applicative_update update)) coroutine_state)
    _->update this_widget

trigger_selector_functor_update::Functor f=>Bool->(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
trigger_selector_functor_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->if bounded then fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (trigger_selector_functor_update bounded update) group_widget) else maybe (update this_widget) (\_->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (trigger_selector_functor_update bounded update) group_widget)) (DIM.lookup index group_widget)
    Vector {index,size,vector_widget}->if bounded then fmap (\new_widget->Vector {index=index,size=size,vector_widget=DV.modify (\this_vector_widget->DVM.write this_vector_widget index new_widget) vector_widget}) (trigger_selector_functor_update bounded update (vector_widget DV.! index)) else maybe (update this_widget) (fmap (\new_widget->Vector {index=index,size=size,vector_widget=DV.modify (\this_vector_widget->DVM.write this_vector_widget index new_widget) vector_widget}) . trigger_selector_functor_update bounded update) (vector_widget DV.!? index)
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->if bounded then fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (trigger_selector_functor_update bounded update)) coroutine_state) else maybe (update this_widget) (\_->fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (trigger_selector_functor_update bounded update)) coroutine_state)) (DIM.lookup index coroutine_state)
    _->update this_widget

abstain_selector_functor_update::Functor f=>Bool->(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
abstain_selector_functor_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->if bounded then fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (abstain_selector_functor_update bounded update) group_widget) else maybe (update this_widget) (\_->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (abstain_selector_functor_update bounded update) group_widget)) (DIM.lookup index group_widget)
    Vector {index,size,vector_widget}->if bounded then fmap (\new_widget->Vector {index=index,size=size,vector_widget=DV.modify (\this_vector_widget->DVM.write this_vector_widget index new_widget) vector_widget}) (abstain_selector_functor_update bounded update (vector_widget DV.! index)) else maybe (update this_widget) (fmap (\new_widget->Vector {index=index,size=size,vector_widget=DV.modify (\this_vector_widget->DVM.write this_vector_widget index new_widget) vector_widget}) . abstain_selector_functor_update bounded update) (vector_widget DV.!? index)
    Widget_trigger {next,widget_trigger,widget}->fmap (\this_this_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=this_this_widget}) (abstain_selector_functor_update bounded update widget)
    Widget_io_trigger {next,widget_io_trigger,widget}->fmap (\this_this_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=this_this_widget}) (abstain_selector_functor_update bounded update widget)
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->fmap (\this_this_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=this_this_widget}) (abstain_selector_functor_update bounded update widget)
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->if bounded then fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (abstain_selector_functor_update bounded update)) coroutine_state) else maybe (update this_widget) (\_->fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (abstain_selector_functor_update bounded update)) coroutine_state)) (DIM.lookup index coroutine_state)
    _->update this_widget

update_coroutine_state::(Widget a b c d e->Widget a b c d e)->Coroutine_state a b c d e->Coroutine_state a b c d e
update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->Coroutine_state {widget=update widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}

functor_update_coroutine_state::Functor f=>(Widget a b c d e->f (Widget a b c d e))->Coroutine_state a b c d e->f (Coroutine_state a b c d e)
functor_update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->fmap (\this_widget->Coroutine_state {widget=this_widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}) (update widget)