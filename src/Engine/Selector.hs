{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Selector where

import Engine.Container
import Engine.Type
import Engine.Underlying
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Control.Monad as CM
import qualified Control.Monad.ST as CMST
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Data.Vector.Storable as DVS

any_group_selector_action::ET.Has_call_stack=>((a->Widget b->c)->a->DIM.IntMap (Widget b)->c)->(Widget b->a->c)->DIM.IntMap (Widget b)->a->c
any_group_selector_action function value group_widget environment=function (flip value) environment group_widget

any_group_selector_update::ET.Has_call_stack=>((DIM.IntMap (Widget a)->Widget a)->b->c)->((Widget a->d)->DIM.IntMap (Widget a)->b)->(Widget a->d)->Int->Int->Int->Int->Int->DIM.IntMap (Widget a)->c
any_group_selector_update wrapper function value initial_min_index min_index initial_max_index max_index index group_widget=wrapper (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (function value group_widget)

hosted_group_selector_action::ET.Has_call_stack=>Bool->(a->b)->(Widget c->a->b)->Int->DIM.IntMap (Widget c)->a->b
hosted_group_selector_action bounded function value index group_widget environment=if bounded then value (int_map_lookup index group_widget) environment else maybe (function environment) (`value` environment) (DIM.lookup index group_widget)

hosted_group_selector_update::ET.Has_call_stack=>Bool->a->((DIM.IntMap (Widget b)->Widget b)->c->a)->(Int->d->DIM.IntMap (Widget b)->c)->d->Int->Int->Int->Int->Int->DIM.IntMap (Widget b)->a
hosted_group_selector_update bounded fallback wrapper function value initial_min_index min_index initial_max_index max_index index group_widget=let result=wrapper (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (function index value group_widget) in if bounded then result else maybe fallback (const result) (DIM.lookup index group_widget)

any_vector_selector_action::ET.Has_call_stack=>((a->Widget b->c)->a->DV.Vector (Widget b)->c)->(Widget b->a->c)->DV.Vector (Widget b)->a->c
any_vector_selector_action function value vector_widget environment=function (flip value) environment vector_widget

any_vector_selector_update::ET.Has_call_stack=>((DV.Vector (Widget a)->Widget a)->b->c)->((Widget a->d)->DV.Vector (Widget a)->b)->(Widget a->d)->Int->DV.Vector (Widget a)->c
any_vector_selector_update wrapper function value index vector_widget=wrapper (\this_vector_widget->Vector {index=index,vector_widget=this_vector_widget}) (function value vector_widget)

hosted_vector_selector_action::ET.Has_call_stack=>Bool->(a->b)->(Widget c->a->b)->Int->DV.Vector (Widget c)->a->b
hosted_vector_selector_action bounded function value index vector_widget environment=if bounded then value (vector_widget DV.! index) environment else maybe (function environment) (`value` environment) (vector_widget DV.!? index)

hosted_vector_selector_update::ET.Has_call_stack=>Bool->a->((DV.Vector (Widget b)->Widget b)->c->a)->(Int->d->DV.Vector (Widget b)->c)->d->Int->DV.Vector (Widget b)->a
hosted_vector_selector_update bounded fallback wrapper function value index vector_widget=let result=wrapper (\this_vector_widget->Vector {index=index,vector_widget=this_vector_widget}) (function index value vector_widget) in if bounded then result else maybe fallback (const result) (vector_widget DV.!? index)

widget_trigger_selector_action::ET.Has_call_stack=>(Widget a->b->c)->Widget a->b->c
widget_trigger_selector_action value=value

widget_trigger_selector_update::ET.Has_call_stack=>((Widget a->Widget a)->b->c)->(d->Widget a->b)->d->(Event a->Engine a->Maybe Int)->Widget a->(Event a->Engine a->Widget a->(Widget a,Engine a->Engine a))->c
widget_trigger_selector_update wrapper function value next widget widget_trigger=wrapper (\this_widget->Widget_trigger {next=next,widget=this_widget,widget_trigger=widget_trigger}) (function value widget)

widget_io_trigger_selector_update::ET.Has_call_stack=>((Widget a->Widget a)->b->c)->(d->Widget a->b)->d->(Event a->Engine a->Maybe Int)->Widget a->(Event a->Engine a->Widget a->(Widget a,Engine a->IO (Engine a)))->c
widget_io_trigger_selector_update wrapper function value next widget widget_io_trigger=wrapper (\this_widget->Widget_io_trigger {next=next,widget=this_widget,widget_io_trigger=widget_io_trigger}) (function value widget)

widget_mix_trigger_selector_update::ET.Has_call_stack=>((Widget a->Widget a)->b->c)->(d->Widget a->b)->d->(Event a->Engine a->Maybe Int)->Widget a->(Event a->Engine a->Widget a->(Widget a,Engine a->Engine a,Engine a->IO (Engine a)))->Bool->c
widget_mix_trigger_selector_update wrapper function value next widget widget_mix_trigger order=wrapper (\this_widget->Widget_mix_trigger {next=next,widget=this_widget,widget_mix_trigger=widget_mix_trigger,order=order}) (function value widget)

any_coroutine_selector_action::ET.Has_call_stack=>((a->Coroutine_state b->c)->a->DIM.IntMap (Coroutine_state b)->c)->(Widget b->a->c)->DIM.IntMap (Coroutine_state b)->a->c
any_coroutine_selector_action function value coroutine_state environment=function (\this_environment single_coroutine_state->value single_coroutine_state.widget this_environment) environment coroutine_state

any_coroutine_selector_update::ET.Has_call_stack=>((DIM.IntMap (Coroutine_state a)->Widget a)->b->c)->(d->DIM.IntMap (Coroutine_state a)->b)->d->Int->Int->Int->Int->Int->Int->Int->DVS.Vector Layout->DV.Vector (Linear_coroutine a)->Bool->DIM.IntMap (Coroutine_state a)->c
any_coroutine_selector_update wrapper function value initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state=wrapper (\this_coroutine_state->Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (function value coroutine_state)

hosted_coroutine_selector_action::ET.Has_call_stack=>Bool->(a->b)->(Widget c->a->b)->Int->DIM.IntMap (Coroutine_state c)->a->b
hosted_coroutine_selector_action bounded function value index coroutine_state environment=if bounded then value (int_map_lookup index coroutine_state).widget environment else maybe (function environment) (\single_coroutine_state->value single_coroutine_state.widget environment) (DIM.lookup index coroutine_state)

hosted_coroutine_selector_update::ET.Has_call_stack=>Bool->a->((DIM.IntMap (Coroutine_state b)->Widget b)->c->a)->(Int->d->DIM.IntMap (Coroutine_state b)->c)->d->Int->Int->Int->Int->Int->Int->Int->DVS.Vector Layout->DV.Vector (Linear_coroutine b)->Bool->DIM.IntMap (Coroutine_state b)->a
hosted_coroutine_selector_update bounded fallback wrapper function value initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state=let result=wrapper (\this_coroutine_state->Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (function index value coroutine_state) in if bounded then result else maybe fallback (const result) (DIM.lookup index coroutine_state)

selector_action::ET.Has_call_stack=>(a->Widget b->c->c)->Selector a->Widget b->c->c
selector_action action this_selector this_widget environment=case this_selector of
    None_selector->environment
    Combine_selector {combine_selector}->DF.foldl' (\this_environment single_selector->selector_action action single_selector this_widget this_environment) environment combine_selector
    Self_selector {value}->action value this_widget environment
    All_selector {maybe_value,value}->all_selector_action (action value) this_widget (selector_action_a maybe_value action this_widget environment)
    Trigger_selector {maybe_value,value,bounded}->trigger_selector_action bounded (action value) this_widget (selector_action_a maybe_value action this_widget environment)
    Default_selector {maybe_value,value,bounded}->default_selector_action bounded (action value) this_widget (selector_action_a maybe_value action this_widget environment)
    Hosted_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {index,group_widget}->let backup=selector_action_a maybe_value action this_widget environment in hosted_group_selector_action bounded (const backup) (selector_action action selector) index group_widget backup
        Vector {index,vector_widget}->let backup=selector_action_a maybe_value action this_widget environment in hosted_vector_selector_action bounded (const backup) (selector_action action selector) index vector_widget backup
        Widget_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget environment)
        Widget_io_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget environment)
        Widget_mix_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget environment)
        Coroutine {index,coroutine_state}->let backup=selector_action_a maybe_value action this_widget environment in hosted_coroutine_selector_action bounded (const backup) (selector_action action selector) index coroutine_state backup
        _->selector_action_b strict maybe_value action this_widget environment
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {group_widget}->any_group_selector_action DIM.foldl' (selector_action action selector) group_widget (selector_action_a maybe_value action this_widget environment)
        Vector {vector_widget}->any_vector_selector_action DF.foldl' (selector_action action selector) vector_widget (selector_action_a maybe_value action this_widget environment)
        Widget_trigger {widget}->widget_trigger_selector_action (selector_action action selector) widget (selector_action_a maybe_value action this_widget environment)
        Widget_io_trigger {widget}->widget_trigger_selector_action (selector_action action selector) widget (selector_action_a maybe_value action this_widget environment)
        Widget_mix_trigger {widget}->widget_trigger_selector_action (selector_action action selector) widget (selector_action_a maybe_value action this_widget environment)
        Coroutine {coroutine_state}->any_coroutine_selector_action DIM.foldl' (selector_action action selector) coroutine_state (selector_action_a maybe_value action this_widget environment)
        _->selector_action_b strict maybe_value action this_widget environment
    Group_selector {maybe_value,group_selector,bounded,strict}->case this_widget of
        Group {group_widget}->DIM.foldlWithKey' (\this_environment index single_selector->if bounded then selector_action action single_selector (int_map_lookup index group_widget) this_environment else maybe this_environment (\widget->selector_action action single_selector widget this_environment) (DIM.lookup index group_widget)) (selector_action_a maybe_value action this_widget environment) group_selector
        _->selector_action_b strict maybe_value action this_widget environment
    Vector_selector {maybe_value,vector_selector,bounded,strict}->case this_widget of
        Vector {vector_widget}->DIM.foldlWithKey' (\this_environment index single_selector->if bounded then selector_action action single_selector (vector_widget DV.! index) this_environment else maybe this_environment (\widget->selector_action action single_selector widget this_environment) (vector_widget DV.!? index)) (selector_action_a maybe_value action this_widget environment) vector_selector
        _->selector_action_b strict maybe_value action this_widget environment
    Widget_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget environment)
        _->selector_action_b strict maybe_value action this_widget environment
    Widget_io_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_io_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget environment)
        _->selector_action_b strict maybe_value action this_widget environment
    Widget_mix_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_mix_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget environment)
        _->selector_action_b strict maybe_value action this_widget environment
    Coroutine_selector {maybe_value,coroutine_selector,bounded,strict}->case this_widget of
        Coroutine {coroutine_state}->DIM.foldlWithKey' (\this_environment index single_selector->if bounded then selector_action action single_selector (int_map_lookup index coroutine_state).widget this_environment else maybe this_environment (\single_coroutine_state->selector_action action single_selector single_coroutine_state.widget this_environment) (DIM.lookup index coroutine_state)) (selector_action_a maybe_value action this_widget environment) coroutine_selector
        _->selector_action_b strict maybe_value action this_widget environment

selector_action_a::ET.Has_call_stack=>Maybe a->(a->Widget b->c->c)->Widget b->c->c
selector_action_a maybe_value action widget environment=case maybe_value of
    Nothing->environment
    Just value->action value widget environment

selector_action_b::ET.Has_call_stack=>Bool->Maybe a->(a->Widget b->c->c)->Widget b->c->c
selector_action_b strict maybe_value action widget environment=if strict then EF.empty_error else case maybe_value of
    Nothing->environment
    Just value->action value widget environment

all_selector_action::ET.Has_call_stack=>(Widget a->b->b)->Widget a->b->b
all_selector_action action this_widget environment=case this_widget of
    Group {group_widget}->any_group_selector_action DIM.foldl' (all_selector_action action) group_widget environment
    Vector {vector_widget}->any_vector_selector_action DF.foldl' (all_selector_action action) vector_widget environment
    Widget_trigger {widget}->widget_trigger_selector_action (all_selector_action action) widget environment
    Widget_io_trigger {widget}->widget_trigger_selector_action (all_selector_action action) widget environment
    Widget_mix_trigger {widget}->widget_trigger_selector_action (all_selector_action action) widget environment
    Coroutine {coroutine_state}->any_coroutine_selector_action DIM.foldl' (all_selector_action action) coroutine_state environment
    _->action this_widget environment

trigger_selector_action::ET.Has_call_stack=>Bool->(Widget a->b->b)->Widget a->b->b
trigger_selector_action bounded action this_widget environment=case this_widget of
    Group {index,group_widget}->hosted_group_selector_action bounded id (trigger_selector_action bounded action) index group_widget environment
    Vector {index,vector_widget}->hosted_vector_selector_action bounded id (trigger_selector_action bounded action) index vector_widget environment
    Coroutine {index,coroutine_state}->hosted_coroutine_selector_action bounded id (trigger_selector_action bounded action) index coroutine_state environment
    _->action this_widget environment

default_selector_action::ET.Has_call_stack=>Bool->(Widget a->b->b)->Widget a->b->b
default_selector_action bounded action this_widget environment=case this_widget of
    Group {index,group_widget}->hosted_group_selector_action bounded id (default_selector_action bounded action) index group_widget environment
    Vector {index,vector_widget}->hosted_vector_selector_action bounded id (default_selector_action bounded action) index vector_widget environment
    Widget_trigger {widget}->widget_trigger_selector_action (default_selector_action bounded action) widget environment
    Widget_io_trigger {widget}->widget_trigger_selector_action (default_selector_action bounded action) widget environment
    Widget_mix_trigger {widget}->widget_trigger_selector_action (default_selector_action bounded action) widget environment
    Coroutine {index,coroutine_state}->hosted_coroutine_selector_action bounded id (default_selector_action bounded action) index coroutine_state environment
    _->action this_widget environment

selector_monad_action::ET.Has_call_stack=>Monad d=>(a->Widget b->c->d c)->Selector a->Widget b->c->d c
selector_monad_action action this_selector this_widget environment=case this_selector of
    None_selector->return environment
    Combine_selector {combine_selector}->DF.foldlM (\this_environment single_selector->selector_monad_action action single_selector this_widget this_environment) environment combine_selector
    Self_selector {value}->action value this_widget environment
    All_selector {maybe_value,value}->selector_monad_action_a maybe_value action this_widget environment (all_selector_monad_action (action value) this_widget)
    Trigger_selector {maybe_value,value,bounded}->selector_monad_action_a maybe_value action this_widget environment (trigger_selector_monad_action bounded (action value) this_widget)
    Default_selector {maybe_value,value,bounded}->selector_monad_action_a maybe_value action this_widget environment (default_selector_monad_action bounded (action value) this_widget)
    Hosted_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {index,group_widget}->selector_monad_action_a maybe_value action this_widget environment (hosted_group_selector_action bounded return (selector_monad_action action selector) index group_widget)
        Vector {index,vector_widget}->selector_monad_action_a maybe_value action this_widget environment (hosted_vector_selector_action bounded return (selector_monad_action action selector) index vector_widget)
        Widget_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Widget_io_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Widget_mix_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Coroutine {index,coroutine_state}->selector_monad_action_a maybe_value action this_widget environment (hosted_coroutine_selector_action bounded return (selector_monad_action action selector) index coroutine_state)
        _->selector_monad_action_b strict maybe_value action this_widget environment
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {group_widget}->selector_monad_action_a maybe_value action this_widget environment (any_group_selector_action DF.foldlM (selector_monad_action action selector) group_widget)
        Vector {vector_widget}->selector_monad_action_a maybe_value action this_widget environment (any_vector_selector_action DF.foldlM (selector_monad_action action selector) vector_widget)
        Widget_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Widget_io_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Widget_mix_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Coroutine {coroutine_state}->selector_monad_action_a maybe_value action this_widget environment (any_coroutine_selector_action DF.foldlM (selector_monad_action action selector) coroutine_state)
        _->selector_monad_action_b strict maybe_value action this_widget environment
    Group_selector {maybe_value,group_selector,bounded,strict}->case this_widget of
        Group {group_widget}->DIM.foldlWithKey' (\this_environment index single_selector->if bounded then this_environment>>=selector_monad_action action single_selector (int_map_lookup index group_widget) else maybe this_environment (\widget->this_environment>>=selector_monad_action action single_selector widget) (DIM.lookup index group_widget)) (selector_monad_action_a maybe_value action this_widget environment return) group_selector
        _->selector_monad_action_b strict maybe_value action this_widget environment
    Vector_selector {maybe_value,vector_selector,bounded,strict}->case this_widget of
        Vector {vector_widget}->DIM.foldlWithKey' (\this_environment index single_selector->if bounded then this_environment>>=selector_monad_action action single_selector (vector_widget DV.! index) else maybe this_environment (\widget->this_environment>>=selector_monad_action action single_selector widget) (vector_widget DV.!? index)) (selector_monad_action_a maybe_value action this_widget environment return) vector_selector
        _->selector_monad_action_b strict maybe_value action this_widget environment
    Widget_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        _->selector_monad_action_b strict maybe_value action this_widget environment
    Widget_io_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_io_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        _->selector_monad_action_b strict maybe_value action this_widget environment
    Widget_mix_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_mix_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        _->selector_monad_action_b strict maybe_value action this_widget environment
    Coroutine_selector {maybe_value,coroutine_selector,bounded,strict}->case this_widget of
        Coroutine {coroutine_state}->DIM.foldlWithKey' (\this_environment index single_selector->if bounded then this_environment>>=selector_monad_action action single_selector (int_map_lookup index coroutine_state).widget else maybe this_environment (\single_coroutine_state->this_environment>>=selector_monad_action action single_selector single_coroutine_state.widget) (DIM.lookup index coroutine_state)) (selector_monad_action_a maybe_value action this_widget environment return) coroutine_selector
        _->selector_monad_action_b strict maybe_value action this_widget environment

selector_monad_action_a::ET.Has_call_stack=>Monad d=>Maybe a->(a->Widget b->c->d c)->Widget b->c->(c->d c)->d c
selector_monad_action_a maybe_value action widget environment monad=case maybe_value of
    Nothing->monad environment
    Just value->do
        new_environment<-action value widget environment
        monad new_environment

selector_monad_action_b::ET.Has_call_stack=>Applicative d=>Bool->Maybe a->(a->Widget b->c->d c)->Widget b->c->d c
selector_monad_action_b strict maybe_value action widget environment=if strict then EF.empty_error else case maybe_value of
    Nothing->pure environment
    Just value->action value widget environment

all_selector_monad_action::ET.Has_call_stack=>Monad c=>(Widget a->b->c b)->Widget a->b->c b
all_selector_monad_action action this_widget environment=case this_widget of
    Group {group_widget}->any_group_selector_action DF.foldlM (all_selector_monad_action action) group_widget environment
    Vector {vector_widget}->any_vector_selector_action DF.foldlM (all_selector_monad_action action) vector_widget environment
    Widget_trigger {widget}->widget_trigger_selector_action (all_selector_monad_action action) widget environment
    Widget_io_trigger {widget}->widget_trigger_selector_action (all_selector_monad_action action) widget environment
    Widget_mix_trigger {widget}->widget_trigger_selector_action (all_selector_monad_action action) widget environment
    Coroutine {coroutine_state}->any_coroutine_selector_action DF.foldlM (all_selector_monad_action action) coroutine_state environment
    _->action this_widget environment

trigger_selector_monad_action::ET.Has_call_stack=>Monad c=>Bool->(Widget a->b->c b)->Widget a->b->c b
trigger_selector_monad_action bounded action this_widget environment=case this_widget of
    Group {index,group_widget}->hosted_group_selector_action bounded return (trigger_selector_monad_action bounded action) index group_widget environment
    Vector {index,vector_widget}->hosted_vector_selector_action bounded return (trigger_selector_monad_action bounded action) index vector_widget environment
    Coroutine {index,coroutine_state}->hosted_coroutine_selector_action bounded return (trigger_selector_monad_action bounded action) index coroutine_state environment
    _->action this_widget environment

default_selector_monad_action::ET.Has_call_stack=>Monad c=>Bool->(Widget a->b->c b)->Widget a->b->c b
default_selector_monad_action bounded action this_widget environment=case this_widget of
    Group {index,group_widget}->hosted_group_selector_action bounded return (default_selector_monad_action bounded action) index group_widget environment
    Vector {index,vector_widget}->hosted_vector_selector_action bounded return (default_selector_monad_action bounded action) index vector_widget environment
    Widget_trigger {widget}->widget_trigger_selector_action (default_selector_monad_action bounded action) widget environment
    Widget_io_trigger {widget}->widget_trigger_selector_action (default_selector_monad_action bounded action) widget environment
    Widget_mix_trigger {widget}->widget_trigger_selector_action (default_selector_monad_action bounded action) widget environment
    Coroutine {index,coroutine_state}->hosted_coroutine_selector_action bounded return (default_selector_monad_action bounded action) index coroutine_state environment
    _->action this_widget environment

selector_update::ET.Has_call_stack=>(a->Widget b->Widget b)->Selector a->Widget b->Widget b
selector_update update this_selector this_widget=case this_selector of
    None_selector->this_widget
    Combine_selector {combine_selector}->DF.foldl' (flip (selector_update update)) this_widget combine_selector
    Self_selector {value}->update value this_widget
    All_selector {maybe_value,value}->selector_update_a maybe_value update (all_selector_update (update value) this_widget)
    Trigger_selector {maybe_value,value,bounded}->selector_update_a maybe_value update (trigger_selector_update bounded (update value) this_widget)
    Default_selector {maybe_value,value,bounded}->selector_update_a maybe_value update (default_selector_update bounded (update value) this_widget)
    Hosted_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (hosted_group_selector_update bounded this_widget id (\this_index this_this_selector this_group_widget->(if bounded then int_map_update else int_map_update_safe) this_index (selector_update update this_this_selector) this_group_widget) selector initial_min_index min_index initial_max_index max_index index group_widget)
        Vector {index,vector_widget}->selector_update_a maybe_value update (hosted_vector_selector_update bounded this_widget id (\this_index this_this_selector this_vector_widget->CMST.runST (action_vector (\this_this_vector_widget->DVM.write this_this_vector_widget this_index (selector_update update this_this_selector (this_vector_widget DV.! this_index))) this_vector_widget)) selector index vector_widget)
        Widget_trigger {next,widget,widget_trigger}->selector_update_a maybe_value update (widget_trigger_selector_update id id (selector_update update selector) next widget widget_trigger)
        Widget_io_trigger {next,widget,widget_io_trigger}->selector_update_a maybe_value update (widget_io_trigger_selector_update id id (selector_update update selector) next widget widget_io_trigger)
        Widget_mix_trigger {next,widget,widget_mix_trigger,order}->selector_update_a maybe_value update (widget_mix_trigger_selector_update id id (selector_update update selector) next widget widget_mix_trigger order)
        Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (hosted_coroutine_selector_update bounded this_widget id (\this_index this_this_selector this_coroutine_state->(if bounded then int_map_update else int_map_update_safe) this_index (update_coroutine_state (selector_update update this_this_selector)) this_coroutine_state) selector initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state)
        _->selector_update_b strict maybe_value update this_widget
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (any_group_selector_update id fmap (selector_update update selector) initial_min_index min_index initial_max_index max_index index group_widget)
        Vector {index,vector_widget}->selector_update_a maybe_value update (any_vector_selector_update id fmap (selector_update update selector) index vector_widget)
        Widget_trigger {next,widget,widget_trigger}->selector_update_a maybe_value update (widget_trigger_selector_update id id (selector_update update selector) next widget widget_trigger)
        Widget_io_trigger {next,widget,widget_io_trigger}->selector_update_a maybe_value update (widget_io_trigger_selector_update id id (selector_update update selector) next widget widget_io_trigger)
        Widget_mix_trigger {next,widget,widget_mix_trigger,order}->selector_update_a maybe_value update (widget_mix_trigger_selector_update id id (selector_update update selector) next widget widget_mix_trigger order)
        Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (any_coroutine_selector_update id fmap (update_coroutine_state (selector_update update selector)) initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state)
        _->selector_update_b strict maybe_value update this_widget
    Group_selector {maybe_value,group_selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=DIM.foldlWithKey' (\this_group_widget this_index single_selector->(if bounded then int_map_update else int_map_update_safe) this_index (selector_update update single_selector) this_group_widget) group_widget group_selector})
        _->selector_update_b strict maybe_value update this_widget
    Vector_selector {maybe_value,vector_selector,bounded,strict}->case this_widget of
        Vector {index,vector_widget}->selector_update_a maybe_value update (Vector {index=index,vector_widget=CMST.runST (action_vector (\this_vector_widget->CM.void (DIM.traverseWithKey (\this_index single_selector->if bounded then DVM.write this_vector_widget this_index (selector_update update single_selector (vector_widget DV.! this_index)) else maybe (return ()) (DVM.write this_vector_widget this_index . selector_update update single_selector) (vector_widget DV.!? this_index)) vector_selector)) vector_widget)})
        _->selector_update_b strict maybe_value update this_widget
    Widget_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_trigger {next,widget,widget_trigger}->selector_update_a maybe_value update (Widget_trigger {next=next,widget=selector_update update selector widget,widget_trigger=widget_trigger})
        _->selector_update_b strict maybe_value update this_widget
    Widget_io_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_io_trigger {next,widget,widget_io_trigger}->selector_update_a maybe_value update (Widget_io_trigger {next=next,widget=selector_update update selector widget,widget_io_trigger=widget_io_trigger})
        _->selector_update_b strict maybe_value update this_widget
    Widget_mix_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_mix_trigger {next,widget,widget_mix_trigger,order}->selector_update_a maybe_value update (Widget_mix_trigger {next=next,widget=selector_update update selector widget,widget_mix_trigger=widget_mix_trigger,order=order})
        _->selector_update_b strict maybe_value update this_widget
    Coroutine_selector {maybe_value,coroutine_selector,bounded,strict}->case this_widget of
        Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=DIM.foldlWithKey' (\this_coroutine_state this_index single_selector->(if bounded then int_map_update else int_map_update_safe) this_index (update_coroutine_state (selector_update update single_selector)) this_coroutine_state) coroutine_state coroutine_selector,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->selector_update_b strict maybe_value update this_widget

selector_update_a::ET.Has_call_stack=>Maybe a->(a->Widget b->Widget b)->Widget b->Widget b
selector_update_a maybe_value update widget=case maybe_value of
    Nothing->widget
    Just value->update value widget

selector_update_b::ET.Has_call_stack=>Bool->Maybe a->(a->Widget b->Widget b)->Widget b->Widget b
selector_update_b strict maybe_value update widget=if strict then EF.empty_error else case maybe_value of
    Nothing->widget
    Just value->update value widget

all_selector_update::ET.Has_call_stack=>(Widget a->Widget a)->Widget a->Widget a
all_selector_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->any_group_selector_update id fmap (all_selector_update update) initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->any_vector_selector_update id fmap (all_selector_update update) index vector_widget
    Widget_trigger {next,widget,widget_trigger}->widget_trigger_selector_update id id (all_selector_update update) next widget widget_trigger
    Widget_io_trigger {next,widget,widget_io_trigger}->widget_io_trigger_selector_update id id (all_selector_update update) next widget widget_io_trigger
    Widget_mix_trigger {next,widget,widget_mix_trigger,order}->widget_mix_trigger_selector_update id id (all_selector_update update) next widget widget_mix_trigger order
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->any_coroutine_selector_update id fmap (update_coroutine_state (all_selector_update update)) initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

trigger_selector_update::ET.Has_call_stack=>Bool->(Widget a->Widget a)->Widget a->Widget a
trigger_selector_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->hosted_group_selector_update bounded this_widget id (\this_index this_update this_group_widget->(if bounded then int_map_update else int_map_update_safe) this_index (trigger_selector_update bounded this_update) this_group_widget) update initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->hosted_vector_selector_update bounded this_widget id (\this_index this_update this_vector_widget->CMST.runST (action_vector (\this_this_vector_widget->DVM.write this_this_vector_widget this_index (trigger_selector_update bounded this_update (this_vector_widget DV.! this_index))) this_vector_widget)) update index vector_widget
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->hosted_coroutine_selector_update bounded this_widget id (\this_index this_update this_coroutine_state->(if bounded then int_map_update else int_map_update_safe) this_index (update_coroutine_state (trigger_selector_update bounded this_update)) this_coroutine_state) update initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

default_selector_update::ET.Has_call_stack=>Bool->(Widget a->Widget a)->Widget a->Widget a
default_selector_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->hosted_group_selector_update bounded this_widget id (\this_index this_update this_group_widget->(if bounded then int_map_update else int_map_update_safe) this_index (default_selector_update bounded this_update) this_group_widget) update initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->hosted_vector_selector_update bounded this_widget id (\this_index this_update this_vector_widget->CMST.runST (action_vector (\this_this_vector_widget->DVM.write this_this_vector_widget this_index (default_selector_update bounded this_update (this_vector_widget DV.! this_index))) this_vector_widget)) update index vector_widget
    Widget_trigger {next,widget,widget_trigger}->widget_trigger_selector_update id id (default_selector_update bounded update) next widget widget_trigger
    Widget_io_trigger {next,widget,widget_io_trigger}->widget_io_trigger_selector_update id id (default_selector_update bounded update) next widget widget_io_trigger
    Widget_mix_trigger {next,widget,widget_mix_trigger,order}->widget_mix_trigger_selector_update id id (default_selector_update bounded update) next widget widget_mix_trigger order
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->hosted_coroutine_selector_update bounded this_widget id (\this_index this_update this_coroutine_state->(if bounded then int_map_update else int_map_update_safe) this_index (update_coroutine_state (default_selector_update bounded this_update)) this_coroutine_state) update initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

selector_monad_update::ET.Has_call_stack=>Monad c=>(a->Widget b->c (Widget b))->Selector a->Widget b->c (Widget b)
selector_monad_update update this_selector this_widget=case this_selector of
    None_selector->return this_widget
    Combine_selector {combine_selector}->DF.foldlM (flip (selector_monad_update update)) this_widget combine_selector
    Self_selector {value}->update value this_widget
    All_selector {maybe_value,value}->selector_monad_update_a maybe_value update (all_selector_applicative_update (update value) this_widget)
    Trigger_selector {maybe_value,value,bounded}->selector_monad_update_a maybe_value update (trigger_selector_applicative_update bounded (update value) this_widget)
    Default_selector {maybe_value,value,bounded}->selector_monad_update_a maybe_value update (default_selector_applicative_update bounded (update value) this_widget)
    Hosted_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_monad_update_a maybe_value update (hosted_group_selector_update bounded (return this_widget) fmap (\this_index this_this_selector this_group_widget->int_map_functor_update this_index (selector_monad_update update this_this_selector) this_group_widget) selector initial_min_index min_index initial_max_index max_index index group_widget)
        Vector {index,vector_widget}->selector_monad_update_a maybe_value update (hosted_vector_selector_update bounded (return this_widget) fmap (\this_index this_this_selector this_vector_widget->fmap (\widget->CMST.runST (action_vector (\this_this_vector_widget->DVM.write this_this_vector_widget this_index widget) this_vector_widget)) (selector_monad_update update this_this_selector (this_vector_widget DV.! this_index))) selector index vector_widget)
        Widget_trigger {next,widget,widget_trigger}->selector_monad_update_a maybe_value update (widget_trigger_selector_update fmap id (selector_monad_update update selector) next widget widget_trigger)
        Widget_io_trigger {next,widget,widget_io_trigger}->selector_monad_update_a maybe_value update (widget_io_trigger_selector_update fmap id (selector_monad_update update selector) next widget widget_io_trigger)
        Widget_mix_trigger {next,widget,widget_mix_trigger,order}->selector_monad_update_a maybe_value update (widget_mix_trigger_selector_update fmap id (selector_monad_update update selector) next widget widget_mix_trigger order)
        Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_monad_update_a maybe_value update (hosted_coroutine_selector_update bounded (return this_widget) fmap (\this_index this_this_selector this_coroutine_state->int_map_functor_update this_index (functor_update_coroutine_state (selector_monad_update update this_this_selector)) this_coroutine_state) selector initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state)
        _->selector_monad_update_b strict maybe_value update this_widget
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_monad_update_a maybe_value update (any_group_selector_update fmap traverse (selector_monad_update update selector) initial_min_index min_index initial_max_index max_index index group_widget)
        Vector {index,vector_widget}->selector_monad_update_a maybe_value update (any_vector_selector_update fmap traverse (selector_monad_update update selector) index vector_widget)
        Widget_trigger {next,widget,widget_trigger}->selector_monad_update_a maybe_value update (widget_trigger_selector_update fmap id (selector_monad_update update selector) next widget widget_trigger)
        Widget_io_trigger {next,widget,widget_io_trigger}->selector_monad_update_a maybe_value update (widget_io_trigger_selector_update fmap id (selector_monad_update update selector) next widget widget_io_trigger)
        Widget_mix_trigger {next,widget,widget_mix_trigger,order}->selector_monad_update_a maybe_value update (widget_mix_trigger_selector_update fmap id (selector_monad_update update selector) next widget widget_mix_trigger order)
        Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_monad_update_a maybe_value update (any_coroutine_selector_update fmap traverse (functor_update_coroutine_state (selector_monad_update update selector)) initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state)
        _->selector_monad_update_b strict maybe_value update this_widget
    Group_selector {maybe_value,group_selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_monad_update_c maybe_value update (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) ((if bounded then int_map_applicative_update else int_map_applicative_update_safe) (selector_monad_update update) group_selector group_widget)
        _->selector_monad_update_b strict maybe_value update this_widget
    Vector_selector {maybe_value,vector_selector,bounded,strict}->case this_widget of
        Vector {index,vector_widget}->selector_monad_update_c maybe_value update (\this_vector_widget->Vector {index=index,vector_widget=CMST.runST (action_vector (\this_this_vector_widget->CM.void (DIM.traverseWithKey (maybe (return ()) . DVM.write this_this_vector_widget) this_vector_widget)) vector_widget)}) (DIM.traverseWithKey (\this_index single_selector->if bounded then fmap Just (selector_monad_update update single_selector (vector_widget DV.! this_index)) else maybe (return Nothing) (fmap Just . selector_monad_update update single_selector) (vector_widget DV.!? this_index)) vector_selector)
        _->selector_monad_update_b strict maybe_value update this_widget
    Widget_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_trigger {next,widget,widget_trigger}->selector_monad_update_c maybe_value update (\this_this_widget->Widget_trigger {next=next,widget=this_this_widget,widget_trigger=widget_trigger}) (selector_monad_update update selector widget)
        _->selector_monad_update_b strict maybe_value update this_widget
    Widget_io_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_io_trigger {next,widget,widget_io_trigger}->selector_monad_update_c maybe_value update (\this_this_widget->Widget_io_trigger {next=next,widget=this_this_widget,widget_io_trigger=widget_io_trigger}) (selector_monad_update update selector widget)
        _->selector_monad_update_b strict maybe_value update this_widget
    Widget_mix_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_mix_trigger {next,widget,widget_mix_trigger,order}->selector_monad_update_c maybe_value update (\this_this_widget->Widget_mix_trigger {next=next,widget=this_this_widget,widget_mix_trigger=widget_mix_trigger,order=order}) (selector_monad_update update selector widget)
        _->selector_monad_update_b strict maybe_value update this_widget
    Coroutine_selector {maybe_value,coroutine_selector,bounded,strict}->case this_widget of
        Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_monad_update_c maybe_value update (\this_coroutine_state->Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) ((if bounded then int_map_applicative_update else int_map_applicative_update_safe) (functor_update_coroutine_state . selector_monad_update update) coroutine_selector coroutine_state)
        _->selector_monad_update_b strict maybe_value update this_widget

selector_monad_update_a::ET.Has_call_stack=>Monad c=>Maybe a->(a->Widget b->c (Widget b))->c (Widget b)->c (Widget b)
selector_monad_update_a maybe_value update applicative_widget=case maybe_value of
    Nothing->applicative_widget
    Just value->do
        widget<-applicative_widget
        update value widget

selector_monad_update_b::ET.Has_call_stack=>Applicative c=>Bool->Maybe a->(a->Widget b->c (Widget b))->Widget b->c (Widget b)
selector_monad_update_b strict maybe_value update widget=if strict then EF.empty_error else case maybe_value of
    Nothing->pure widget
    Just value->update value widget

selector_monad_update_c::ET.Has_call_stack=>Monad c=>Maybe a->(a->Widget b->c (Widget b))->(d->Widget b)->c d->c (Widget b)
selector_monad_update_c maybe_value update function applicative_value=case maybe_value of
    Nothing->fmap function applicative_value
    Just value->do
        new_value<-applicative_value
        update value (function new_value)

all_selector_applicative_update::ET.Has_call_stack=>Applicative b=>(Widget a->b (Widget a))->Widget a->b (Widget a)
all_selector_applicative_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->any_group_selector_update fmap traverse (all_selector_applicative_update update) initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->any_vector_selector_update fmap traverse (all_selector_applicative_update update) index vector_widget
    Widget_trigger {next,widget,widget_trigger}->widget_trigger_selector_update fmap id (all_selector_applicative_update update) next widget widget_trigger
    Widget_io_trigger {next,widget,widget_io_trigger}->widget_io_trigger_selector_update fmap id (all_selector_applicative_update update) next widget widget_io_trigger
    Widget_mix_trigger {next,widget,widget_mix_trigger,order}->widget_mix_trigger_selector_update fmap id (all_selector_applicative_update update) next widget widget_mix_trigger order
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->any_coroutine_selector_update fmap traverse (functor_update_coroutine_state (all_selector_applicative_update update)) initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

trigger_selector_applicative_update::ET.Has_call_stack=>Applicative b=>Bool->(Widget a->b (Widget a))->Widget a->b (Widget a)
trigger_selector_applicative_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->hosted_group_selector_update bounded (pure this_widget) fmap (\this_index this_update this_group_widget->int_map_functor_update this_index (trigger_selector_applicative_update bounded this_update) this_group_widget) update initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->hosted_vector_selector_update bounded (pure this_widget) fmap (\this_index this_update this_vector_widget->fmap (\widget->CMST.runST (action_vector (\this_this_vector_widget->DVM.write this_this_vector_widget this_index widget) this_vector_widget)) (trigger_selector_applicative_update bounded this_update (this_vector_widget DV.! this_index))) update index vector_widget
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->hosted_coroutine_selector_update bounded (pure this_widget) fmap (\this_index this_update this_coroutine_state->int_map_functor_update this_index (functor_update_coroutine_state (trigger_selector_applicative_update bounded this_update)) this_coroutine_state) update initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

default_selector_applicative_update::ET.Has_call_stack=>Applicative b=>Bool->(Widget a->b (Widget a))->Widget a->b (Widget a)
default_selector_applicative_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->hosted_group_selector_update bounded (pure this_widget) fmap (\this_index this_update this_group_widget->int_map_functor_update this_index (default_selector_applicative_update bounded this_update) this_group_widget) update initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->hosted_vector_selector_update bounded (pure this_widget) fmap (\this_index this_update this_vector_widget->fmap (\widget->CMST.runST (action_vector (\this_this_vector_widget->DVM.write this_this_vector_widget this_index widget) this_vector_widget)) (default_selector_applicative_update bounded this_update (this_vector_widget DV.! this_index))) update index vector_widget
    Widget_trigger {next,widget,widget_trigger}->widget_trigger_selector_update fmap id (default_selector_applicative_update bounded update) next widget widget_trigger
    Widget_io_trigger {next,widget,widget_io_trigger}->widget_io_trigger_selector_update fmap id (default_selector_applicative_update bounded update) next widget widget_io_trigger
    Widget_mix_trigger {next,widget,widget_mix_trigger,order}->widget_mix_trigger_selector_update fmap id (default_selector_applicative_update bounded update) next widget widget_mix_trigger order
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->hosted_coroutine_selector_update bounded (pure this_widget) fmap (\this_index this_update this_coroutine_state->int_map_functor_update this_index (functor_update_coroutine_state (default_selector_applicative_update bounded this_update)) this_coroutine_state) update initial_min_index min_index initial_max_index max_index index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

any_group_visual_selector_action::ET.Has_call_stack=>((a->Visual b->c)->a->DIM.IntMap (Visual b)->c)->(Visual b->a->c)->DIM.IntMap (Visual b)->a->c
any_group_visual_selector_action function value group_visual environment=function (flip value) environment group_visual

any_group_visual_selector_update::ET.Has_call_stack=>((DIM.IntMap (Visual a)->Widget a)->b->c)->((Visual a->d)->DIM.IntMap (Visual a)->b)->(Visual a->d)->Arrange->DIM.IntMap (Visual a)->c
any_group_visual_selector_update wrapper function value arrange group_visual=wrapper (\this_group_visual->Group_visual {arrange=arrange,group_visual=this_group_visual}) (function value group_visual)

any_vector_visual_selector_action::ET.Has_call_stack=>((a->Visual b->c)->a->DV.Vector (Visual b)->c)->(Visual b->a->c)->DV.Vector (Visual b)->a->c
any_vector_visual_selector_action function value vector_visual environment=function (flip value) environment vector_visual

any_vector_visual_selector_update::ET.Has_call_stack=>((DV.Vector (Visual a)->Widget a)->b->c)->((Visual a->d)->DV.Vector (Visual a)->b)->(Visual a->d)->Arrange->DV.Vector (Visual a)->c
any_vector_visual_selector_update wrapper function value arrange vector_visual=wrapper (\this_vector_visual->Vector_visual {arrange=arrange,vector_visual=this_vector_visual}) (function value vector_visual)

visual_selector_action::ET.Has_call_stack=>(a->(Arrange->Arrange)->Visual b->c->c)->Visual_selector a->Widget b->c->c
visual_selector_action action visual_selector widget environment=case visual_selector of
    None_visual_selector->environment
    Combine_visual_selector {combine_visual_selector}->DF.foldl' (\this_environment single_selector->visual_selector_action action single_selector widget this_environment) environment combine_visual_selector
    Any_visual_selector {value,strict}->any_visual_selector_action strict (action value) widget environment
    Visual_trigger_selector {value,strict}->case widget of
        Visual_trigger {visual}->action value id visual environment
        _->if strict then EF.empty_error else environment
    Visual_io_trigger_selector {value,strict}->case widget of
        Visual_io_trigger {visual}->action value id visual environment
        _->if strict then EF.empty_error else environment
    Visual_mix_trigger_selector {value,strict}->case widget of
        Visual_mix_trigger {visual}->action value id visual environment
        _->if strict then EF.empty_error else environment
    Group_visual_selector {group_value,bounded,strict}->case widget of
        Group_visual {arrange,group_visual}->DIM.foldlWithKey' (\this_environment index value->if bounded then action value (combine_arrange arrange) (int_map_lookup index group_visual) this_environment else maybe this_environment (\visual->action value (combine_arrange arrange) visual this_environment) (DIM.lookup index group_visual)) environment group_value
        _->if strict then EF.empty_error else environment
    Vector_visual_selector {vector_value,bounded,strict}->case widget of
        Vector_visual {arrange,vector_visual}->DIM.foldlWithKey' (\this_environment index value->if bounded then action value (combine_arrange arrange) (vector_visual DV.! index) this_environment else maybe this_environment (\visual->action value (combine_arrange arrange) visual this_environment) (vector_visual DV.!? index)) environment vector_value
        _->if strict then EF.empty_error else environment

any_visual_selector_action::ET.Has_call_stack=>Bool->((Arrange->Arrange)->Visual a->b->b)->Widget a->b->b
any_visual_selector_action strict action widget environment=case widget of
    Visual_trigger {visual}->action id visual environment
    Visual_io_trigger {visual}->action id visual environment
    Visual_mix_trigger {visual}->action id visual environment
    Group_visual {arrange,group_visual}->any_group_visual_selector_action DIM.foldl' (action (combine_arrange arrange)) group_visual environment
    Vector_visual {arrange,vector_visual}->any_vector_visual_selector_action DF.foldl' (action (combine_arrange arrange)) vector_visual environment
    _->if strict then EF.empty_error else environment

visual_selector_monad_action::ET.Has_call_stack=>Monad d=>(a->(Arrange->Arrange)->Visual b->c->d c)->Visual_selector a->Widget b->c->d c
visual_selector_monad_action action visual_selector widget environment=case visual_selector of
    None_visual_selector->return environment
    Combine_visual_selector {combine_visual_selector}->DF.foldlM (\this_environment single_selector->visual_selector_monad_action action single_selector widget this_environment) environment combine_visual_selector
    Any_visual_selector {value,strict}->any_visual_selector_monad_action strict (action value) widget environment
    Visual_trigger_selector {value,strict}->case widget of
        Visual_trigger {visual}->action value id visual environment
        _->if strict then EF.empty_error else return environment
    Visual_io_trigger_selector {value,strict}->case widget of
        Visual_io_trigger {visual}->action value id visual environment
        _->if strict then EF.empty_error else return environment
    Visual_mix_trigger_selector {value,strict}->case widget of
        Visual_mix_trigger {visual}->action value id visual environment
        _->if strict then EF.empty_error else return environment
    Group_visual_selector {group_value,bounded,strict}->case widget of
        Group_visual {arrange,group_visual}->DIM.foldlWithKey' (\this_environment index value->if bounded then this_environment>>=action value (combine_arrange arrange) (int_map_lookup index group_visual) else maybe this_environment (\visual->this_environment>>=action value (combine_arrange arrange) visual) (DIM.lookup index group_visual)) (return environment) group_value
        _->if strict then EF.empty_error else return environment
    Vector_visual_selector {vector_value,bounded,strict}->case widget of
        Vector_visual {arrange,vector_visual}->DIM.foldlWithKey' (\this_environment index value->if bounded then this_environment>>=action value (combine_arrange arrange) (vector_visual DV.! index) else maybe this_environment (\visual->this_environment>>=action value (combine_arrange arrange) visual) (vector_visual DV.!? index)) (return environment) vector_value
        _->if strict then EF.empty_error else return environment

any_visual_selector_monad_action::ET.Has_call_stack=>Monad c=>Bool->((Arrange->Arrange)->Visual a->b->c b)->Widget a->b->c b
any_visual_selector_monad_action strict action widget environment=case widget of
    Visual_trigger {visual}->action id visual environment
    Visual_io_trigger {visual}->action id visual environment
    Visual_mix_trigger {visual}->action id visual environment
    Group_visual {arrange,group_visual}->any_group_visual_selector_action DF.foldlM (action (combine_arrange arrange)) group_visual environment
    Vector_visual {arrange,vector_visual}->any_vector_visual_selector_action DF.foldlM (action (combine_arrange arrange)) vector_visual environment
    _->if strict then EF.empty_error else return environment

visual_selector_update::ET.Has_call_stack=>(a->(Arrange->Arrange)->Visual b->Visual b)->Visual_selector a->Widget b->Widget b
visual_selector_update update visual_selector widget=case visual_selector of
    None_visual_selector->widget
    Combine_visual_selector {combine_visual_selector}->DF.foldl' (flip (visual_selector_update update)) widget combine_visual_selector
    Any_visual_selector {value,strict}->any_visual_selector_update strict (update value) widget
    Visual_trigger_selector {value,strict}->case widget of
        Visual_trigger {next,visual,visual_trigger}->Visual_trigger {next=next,visual=update value id visual,visual_trigger=visual_trigger}
        _->if strict then EF.empty_error else widget
    Visual_io_trigger_selector {value,strict}->case widget of
        Visual_io_trigger {next,visual,visual_io_trigger}->Visual_io_trigger {next=next,visual=update value id visual,visual_io_trigger=visual_io_trigger}
        _->if strict then EF.empty_error else widget
    Visual_mix_trigger_selector {value,strict}->case widget of
        Visual_mix_trigger {next,visual,visual_mix_trigger,order}->Visual_mix_trigger {next=next,visual=update value id visual,visual_mix_trigger=visual_mix_trigger,order=order}
        _->if strict then EF.empty_error else widget
    Group_visual_selector {group_value,bounded,strict}->case widget of
        Group_visual {arrange,group_visual}->Group_visual {arrange=arrange,group_visual=DIM.foldlWithKey' (\this_group_visual index value->(if bounded then int_map_update else int_map_update_safe) index (update value (combine_arrange arrange)) this_group_visual) group_visual group_value}
        _->if strict then EF.empty_error else widget
    Vector_visual_selector {vector_value,bounded,strict}->case widget of
        Vector_visual {arrange,vector_visual}->Vector_visual {arrange=arrange,vector_visual=CMST.runST (action_vector (\this_vector_visual->CM.void (DIM.traverseWithKey (\index value->if bounded then DVM.write this_vector_visual index (update value (combine_arrange arrange) (vector_visual DV.! index)) else maybe (return ()) (DVM.write this_vector_visual index . update value (combine_arrange arrange)) (vector_visual DV.!? index)) vector_value)) vector_visual)}
        _->if strict then EF.empty_error else widget

any_visual_selector_update::ET.Has_call_stack=>Bool->((Arrange->Arrange)->Visual a->Visual a)->Widget a->Widget a
any_visual_selector_update strict update widget=case widget of
    Visual_trigger {next,visual,visual_trigger}->Visual_trigger {next=next,visual=update id visual,visual_trigger=visual_trigger}
    Visual_io_trigger {next,visual,visual_io_trigger}->Visual_io_trigger {next=next,visual=update id visual,visual_io_trigger=visual_io_trigger}
    Visual_mix_trigger {next,visual,visual_mix_trigger,order}->Visual_mix_trigger {next=next,visual=update id visual,visual_mix_trigger=visual_mix_trigger,order=order}
    Group_visual {arrange,group_visual}->any_group_visual_selector_update id fmap (update (combine_arrange arrange)) arrange group_visual
    Vector_visual {arrange,vector_visual}->any_vector_visual_selector_update id fmap (update (combine_arrange arrange)) arrange vector_visual
    _->if strict then EF.empty_error else widget

visual_selector_monad_update::ET.Has_call_stack=>Monad c=>(a->(Arrange->Arrange)->Visual b->c (Visual b))->Visual_selector a->Widget b->c (Widget b)
visual_selector_monad_update update visual_selector widget=case visual_selector of
    None_visual_selector->return widget
    Combine_visual_selector {combine_visual_selector}->DF.foldlM (flip (visual_selector_monad_update update)) widget combine_visual_selector
    Any_visual_selector {value,strict}->any_visual_selector_applicative_update strict (update value) widget
    Visual_trigger_selector {value,strict}->case widget of
        Visual_trigger {next,visual,visual_trigger}->fmap (\this_visual->Visual_trigger {next=next,visual=this_visual,visual_trigger=visual_trigger}) (update value id visual)
        _->if strict then EF.empty_error else pure widget
    Visual_io_trigger_selector {value,strict}->case widget of
        Visual_io_trigger {next,visual,visual_io_trigger}->fmap (\this_visual->Visual_io_trigger {next=next,visual=this_visual,visual_io_trigger=visual_io_trigger}) (update value id visual)
        _->if strict then EF.empty_error else pure widget
    Visual_mix_trigger_selector {value,strict}->case widget of
        Visual_mix_trigger {next,visual,visual_mix_trigger,order}->fmap (\this_visual->Visual_mix_trigger {next=next,visual=this_visual,visual_mix_trigger=visual_mix_trigger,order=order}) (update value id visual)
        _->if strict then EF.empty_error else pure widget
    Group_visual_selector {group_value,bounded,strict}->case widget of
        Group_visual {arrange,group_visual}->fmap (\this_group_visual->Group_visual {arrange=arrange,group_visual=this_group_visual}) ((if bounded then int_map_applicative_update else int_map_applicative_update_safe) (\value->update value (combine_arrange arrange)) group_value group_visual)
        _->if strict then EF.empty_error else pure widget
    Vector_visual_selector {vector_value,bounded,strict}->case widget of
        Vector_visual {arrange,vector_visual}->fmap (\this_vector_visual->Vector_visual {arrange=arrange,vector_visual=CMST.runST (action_vector (\this_this_vector_visual->CM.void (DIM.traverseWithKey (maybe (return ()) . DVM.write this_this_vector_visual) this_vector_visual)) vector_visual)}) (DIM.traverseWithKey (\index value->if bounded then fmap Just (update value (combine_arrange arrange) (vector_visual DV.! index)) else maybe (return Nothing) (fmap Just . update value (combine_arrange arrange)) (vector_visual DV.!? index)) vector_value)
        _->if strict then EF.empty_error else pure widget

any_visual_selector_applicative_update::ET.Has_call_stack=>Applicative b=>Bool->((Arrange->Arrange)->Visual a->b (Visual a))->Widget a->b (Widget a)
any_visual_selector_applicative_update strict update widget=case widget of
    Visual_trigger {next,visual,visual_trigger}->fmap (\this_visual->Visual_trigger {next=next,visual=this_visual,visual_trigger=visual_trigger}) (update id visual)
    Visual_io_trigger {next,visual,visual_io_trigger}->fmap (\this_visual->Visual_io_trigger {next=next,visual=this_visual,visual_io_trigger=visual_io_trigger}) (update id visual)
    Visual_mix_trigger {next,visual,visual_mix_trigger,order}->fmap (\this_visual->Visual_mix_trigger {next=next,visual=this_visual,visual_mix_trigger=visual_mix_trigger,order=order}) (update id visual)
    Group_visual {arrange,group_visual}->any_group_visual_selector_update fmap traverse (update (combine_arrange arrange)) arrange group_visual
    Vector_visual {arrange,vector_visual}->any_vector_visual_selector_update fmap traverse (update (combine_arrange arrange)) arrange vector_visual
    _->if strict then EF.empty_error else pure widget

{-# INLINE any_group_selector_action #-}
{-# INLINE any_group_selector_update #-}
{-# INLINE hosted_group_selector_action #-}
{-# INLINE hosted_group_selector_update #-}
{-# INLINE any_vector_selector_action #-}
{-# INLINE any_vector_selector_update #-}
{-# INLINE hosted_vector_selector_action #-}
{-# INLINE hosted_vector_selector_update #-}
{-# INLINE widget_trigger_selector_action #-}
{-# INLINE widget_trigger_selector_update #-}
{-# INLINE widget_io_trigger_selector_update #-}
{-# INLINE widget_mix_trigger_selector_update #-}
{-# INLINE any_coroutine_selector_action #-}
{-# INLINE any_coroutine_selector_update #-}
{-# INLINE hosted_coroutine_selector_action #-}
{-# INLINE hosted_coroutine_selector_update #-}
{-# INLINE selector_action #-}
{-# INLINE selector_action_a #-}
{-# INLINE selector_action_b #-}
{-# INLINE all_selector_action #-}
{-# INLINE trigger_selector_action #-}
{-# INLINE default_selector_action #-}
{-# INLINE selector_monad_action #-}
{-# INLINE selector_monad_action_a #-}
{-# INLINE selector_monad_action_b #-}
{-# INLINE all_selector_monad_action #-}
{-# INLINE trigger_selector_monad_action #-}
{-# INLINE default_selector_monad_action #-}
{-# INLINE selector_update #-}
{-# INLINE selector_update_a #-}
{-# INLINE selector_update_b #-}
{-# INLINE all_selector_update #-}
{-# INLINE trigger_selector_update #-}
{-# INLINE default_selector_update #-}
{-# INLINE selector_monad_update #-}
{-# INLINE selector_monad_update_a #-}
{-# INLINE selector_monad_update_b #-}
{-# INLINE selector_monad_update_c #-}
{-# INLINE all_selector_applicative_update #-}
{-# INLINE trigger_selector_applicative_update #-}
{-# INLINE default_selector_applicative_update #-}
{-# INLINE any_group_visual_selector_action #-}
{-# INLINE any_group_visual_selector_update #-}
{-# INLINE any_vector_visual_selector_action #-}
{-# INLINE any_vector_visual_selector_update #-}
{-# INLINE visual_selector_action #-}
{-# INLINE any_visual_selector_action #-}
{-# INLINE visual_selector_monad_action #-}
{-# INLINE any_visual_selector_monad_action #-}
{-# INLINE visual_selector_update #-}
{-# INLINE any_visual_selector_update #-}
{-# INLINE visual_selector_monad_update #-}
{-# INLINE any_visual_selector_applicative_update #-}