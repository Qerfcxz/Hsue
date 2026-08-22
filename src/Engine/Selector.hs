{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Selector where

import Engine.Container
import Engine.Operation
import Engine.Type
import qualified Error.Error as EE
import qualified Control.Monad as CM
import qualified Control.Monad.ST as CMST
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Data.Vector.Storable as DVS

group_all_selector_action::((a->Widget b c d e f->g)->a->DIM.IntMap (Widget b c d e f)->g)->(Widget b c d e f->a->g)->DIM.IntMap (Widget b c d e f)->a->g
group_all_selector_action function value group_widget environment=function (flip value) environment group_widget

group_all_selector_update::((DIM.IntMap (Widget a b c d e)->Widget a b c d e)->f->g)->((Widget a b c d e->h)->DIM.IntMap (Widget a b c d e)->f)->(Widget a b c d e->h)->Int->Int->Int->Int->Int->DIM.IntMap (Widget a b c d e)->g
group_all_selector_update wrapper function value initial_min_index min_index initial_max_index max_index index group_widget=wrapper (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (function value group_widget)

group_default_selector_action::Bool->(a->b)->(Widget c d e f g->a->b)->Int->DIM.IntMap (Widget c d e f g)->a->b
group_default_selector_action bounded function value index group_widget environment=if bounded then value (int_map_lookup index group_widget) environment else maybe (function environment) (`value` environment) (DIM.lookup index group_widget)

group_default_selector_update::Bool->a->((DIM.IntMap (Widget b c d e f)->Widget b c d e f)->g->a)->(Int->h->DIM.IntMap (Widget b c d e f)->g)->h->Int->Int->Int->Int->Int->DIM.IntMap (Widget b c d e f)->a
group_default_selector_update bounded fallback wrapper function value initial_min_index min_index initial_max_index max_index index group_widget=let result=wrapper (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (function index value group_widget) in if bounded then result else maybe fallback (const result) (DIM.lookup index group_widget)

vector_all_selector_action::((a->Widget b c d e f->g)->a->DV.Vector (Widget b c d e f)->g)->(Widget b c d e f->a->g)->DV.Vector (Widget b c d e f)->a->g
vector_all_selector_action function value vector_widget environment=function (flip value) environment vector_widget

vector_all_selector_update::((DV.Vector (Widget a b c d e)->Widget a b c d e)->f->g)->((Widget a b c d e->h)->DV.Vector (Widget a b c d e)->f)->(Widget a b c d e->h)->Int->DV.Vector (Widget a b c d e)->g
vector_all_selector_update wrapper function value index vector_widget=wrapper (\this_vector_widget->Vector {index=index,vector_widget=this_vector_widget}) (function value vector_widget)

vector_default_selector_action::Bool->(a->b)->(Widget c d e f g->a->b)->Int->DV.Vector (Widget c d e f g)->a->b
vector_default_selector_action bounded function value index vector_widget environment=if bounded then value (vector_widget DV.! index) environment else maybe (function environment) (`value` environment) (vector_widget DV.!? index)

vector_default_selector_update::Bool->a->((DV.Vector (Widget b c d e f)->Widget b c d e f)->g->a)->(Int->h->DV.Vector (Widget b c d e f)->g)->h->Int->DV.Vector (Widget b c d e f)->a
vector_default_selector_update bounded fallback wrapper function value index vector_widget=let result=wrapper (\this_vector_widget->Vector {index=index,vector_widget=this_vector_widget}) (function index value vector_widget) in if bounded then result else maybe fallback (const result) (vector_widget DV.!? index)

widget_trigger_selector_action::(Widget a b c d e->f->g)->Widget a b c d e->f->g
widget_trigger_selector_action value=value

widget_trigger_selector_update::((Widget a b c d e->Widget a b c d e)->f->g)->(h->Widget a b c d e->f)->h->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e))->Widget a b c d e->g
widget_trigger_selector_update wrapper function value next widget_trigger widget=wrapper (\this_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=this_widget}) (function value widget)

widget_io_trigger_selector_update::((Widget a b c d e->Widget a b c d e)->f->g)->(h->Widget a b c d e->f)->h->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->IO (Engine a b c d e)))->Widget a b c d e->g
widget_io_trigger_selector_update wrapper function value next widget_io_trigger widget=wrapper (\this_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=this_widget}) (function value widget)

widget_mix_trigger_selector_update::((Widget a b c d e->Widget a b c d e)->f->g)->(h->Widget a b c d e->f)->h->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e,Engine a b c d e->IO (Engine a b c d e)))->Bool->Widget a b c d e->g
widget_mix_trigger_selector_update wrapper function value next widget_mix_trigger order widget=wrapper (\this_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=this_widget}) (function value widget)

coroutine_all_selector_action::((a->Coroutine_state b c d e f->g)->a->DIM.IntMap (Coroutine_state b c d e f)->g)->(Widget b c d e f->a->g)->DIM.IntMap (Coroutine_state b c d e f)->a->g
coroutine_all_selector_action function value coroutine_state environment=function (\this_environment single_coroutine_state->value single_coroutine_state.widget this_environment) environment coroutine_state

coroutine_all_selector_update::((DIM.IntMap (Coroutine_state a b c d e)->Widget a b c d e)->f->g)->(h->DIM.IntMap (Coroutine_state a b c d e)->f)->h->Int->Int->Int->Int->Int->Int->Int->DVS.Vector Layout->DV.Vector (Linear_coroutine a b c d e)->Bool->DIM.IntMap (Coroutine_state a b c d e)->g
coroutine_all_selector_update wrapper function value index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state=wrapper (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (function value coroutine_state)

coroutine_default_selector_action::Bool->(a->b)->(Widget c d e f g->a->b)->Int->DIM.IntMap (Coroutine_state c d e f g)->a->b
coroutine_default_selector_action bounded function value index coroutine_state environment=if bounded then value (int_map_lookup index coroutine_state).widget environment else maybe (function environment) (\single_coroutine_state->value single_coroutine_state.widget environment) (DIM.lookup index coroutine_state)

coroutine_default_selector_update::Bool->a->((DIM.IntMap (Coroutine_state b c d e f)->Widget b c d e f)->g->a)->(Int->h->DIM.IntMap (Coroutine_state b c d e f)->g)->h->Int->Int->Int->Int->Int->Int->Int->DVS.Vector Layout->DV.Vector (Linear_coroutine b c d e f)->Bool->DIM.IntMap (Coroutine_state b c d e f)->a
coroutine_default_selector_update bounded fallback wrapper function value index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state=let result=wrapper (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (function index value coroutine_state) in if bounded then result else maybe fallback (const result) (DIM.lookup index coroutine_state)

selector_action::(a->Widget b c d e f->g->g)->Selector a->Widget b c d e f->g->g
selector_action action this_selector this_widget environment=case this_selector of
    None_selector->environment
    Combine_selector {combine_selector}->DF.foldl' (\this_environment single_selector->selector_action action single_selector this_widget this_environment) environment combine_selector
    Self_selector {value}->action value this_widget environment
    All_selector {maybe_value,value}->all_selector_action (action value) this_widget (selector_action_a maybe_value action this_widget environment)
    Trigger_selector {maybe_value,value,bounded}->trigger_selector_action bounded (action value) this_widget (selector_action_a maybe_value action this_widget environment)
    Default_selector {maybe_value,value,bounded}->default_selector_action bounded (action value) this_widget (selector_action_a maybe_value action this_widget environment)
    Hosted_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {index,group_widget}->let backup=selector_action_a maybe_value action this_widget environment in group_default_selector_action bounded (const backup) (selector_action action selector) index group_widget backup
        Vector {index,vector_widget}->let backup=selector_action_a maybe_value action this_widget environment in vector_default_selector_action bounded (const backup) (selector_action action selector) index vector_widget backup
        Widget_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget environment)
        Widget_io_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget environment)
        Widget_mix_trigger {widget}->selector_action action selector widget (selector_action_a maybe_value action this_widget environment)
        Coroutine {index,coroutine_state}->let backup=selector_action_a maybe_value action this_widget environment in coroutine_default_selector_action bounded (const backup) (selector_action action selector) index coroutine_state backup
        _->selector_action_b strict maybe_value action this_widget environment
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {group_widget}->group_all_selector_action DIM.foldl' (selector_action action selector) group_widget (selector_action_a maybe_value action this_widget environment)
        Vector {vector_widget}->vector_all_selector_action DF.foldl' (selector_action action selector) vector_widget (selector_action_a maybe_value action this_widget environment)
        Widget_trigger {widget}->widget_trigger_selector_action (selector_action action selector) widget (selector_action_a maybe_value action this_widget environment)
        Widget_io_trigger {widget}->widget_trigger_selector_action (selector_action action selector) widget (selector_action_a maybe_value action this_widget environment)
        Widget_mix_trigger {widget}->widget_trigger_selector_action (selector_action action selector) widget (selector_action_a maybe_value action this_widget environment)
        Coroutine {coroutine_state}->coroutine_all_selector_action DIM.foldl' (selector_action action selector) coroutine_state (selector_action_a maybe_value action this_widget environment)
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

selector_action_a::Maybe a->(a->Widget b c d e f->g->g)->Widget b c d e f->g->g
selector_action_a maybe_value action widget environment=case maybe_value of
    Nothing->environment
    Just value->action value widget environment

selector_action_b::Bool->Maybe a->(a->Widget b c d e f->g->g)->Widget b c d e f->g->g
selector_action_b strict maybe_value action widget environment=if strict then EE.empty_error else case maybe_value of
    Nothing->environment
    Just value->action value widget environment

all_selector_action::(Widget a b c d e->f->f)->Widget a b c d e->f->f
all_selector_action action this_widget environment=case this_widget of
    Group {group_widget}->group_all_selector_action DIM.foldl' (all_selector_action action) group_widget environment
    Vector {vector_widget}->vector_all_selector_action DF.foldl' (all_selector_action action) vector_widget environment
    Widget_trigger {widget}->widget_trigger_selector_action (all_selector_action action) widget environment
    Widget_io_trigger {widget}->widget_trigger_selector_action (all_selector_action action) widget environment
    Widget_mix_trigger {widget}->widget_trigger_selector_action (all_selector_action action) widget environment
    Coroutine {coroutine_state}->coroutine_all_selector_action DIM.foldl' (all_selector_action action) coroutine_state environment
    _->action this_widget environment

trigger_selector_action::Bool->(Widget a b c d e->f->f)->Widget a b c d e->f->f
trigger_selector_action bounded action this_widget environment=case this_widget of
    Group {index,group_widget}->group_default_selector_action bounded id (trigger_selector_action bounded action) index group_widget environment
    Vector {index,vector_widget}->vector_default_selector_action bounded id (trigger_selector_action bounded action) index vector_widget environment
    Coroutine {index,coroutine_state}->coroutine_default_selector_action bounded id (trigger_selector_action bounded action) index coroutine_state environment
    _->action this_widget environment

default_selector_action::Bool->(Widget a b c d e->f->f)->Widget a b c d e->f->f
default_selector_action bounded action this_widget environment=case this_widget of
    Group {index,group_widget}->group_default_selector_action bounded id (default_selector_action bounded action) index group_widget environment
    Vector {index,vector_widget}->vector_default_selector_action bounded id (default_selector_action bounded action) index vector_widget environment
    Widget_trigger {widget}->widget_trigger_selector_action (default_selector_action bounded action) widget environment
    Widget_io_trigger {widget}->widget_trigger_selector_action (default_selector_action bounded action) widget environment
    Widget_mix_trigger {widget}->widget_trigger_selector_action (default_selector_action bounded action) widget environment
    Coroutine {index,coroutine_state}->coroutine_default_selector_action bounded id (default_selector_action bounded action) index coroutine_state environment
    _->action this_widget environment

selector_monad_action::Monad h=>(a->Widget b c d e f->g->h g)->Selector a->Widget b c d e f->g->h g
selector_monad_action action this_selector this_widget environment=case this_selector of
    None_selector->return environment
    Combine_selector {combine_selector}->DF.foldlM (\this_environment single_selector->selector_monad_action action single_selector this_widget this_environment) environment combine_selector
    Self_selector {value}->action value this_widget environment
    All_selector {maybe_value,value}->selector_monad_action_a maybe_value action this_widget environment (all_selector_monad_action (action value) this_widget)
    Trigger_selector {maybe_value,value,bounded}->selector_monad_action_a maybe_value action this_widget environment (trigger_selector_monad_action bounded (action value) this_widget)
    Default_selector {maybe_value,value,bounded}->selector_monad_action_a maybe_value action this_widget environment (default_selector_monad_action bounded (action value) this_widget)
    Hosted_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {index,group_widget}->selector_monad_action_a maybe_value action this_widget environment (group_default_selector_action bounded return (selector_monad_action action selector) index group_widget)
        Vector {index,vector_widget}->selector_monad_action_a maybe_value action this_widget environment (vector_default_selector_action bounded return (selector_monad_action action selector) index vector_widget)
        Widget_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Widget_io_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Widget_mix_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Coroutine {index,coroutine_state}->selector_monad_action_a maybe_value action this_widget environment (coroutine_default_selector_action bounded return (selector_monad_action action selector) index coroutine_state)
        _->selector_monad_action_b strict maybe_value action this_widget environment
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {group_widget}->selector_monad_action_a maybe_value action this_widget environment (group_all_selector_action DF.foldlM (selector_monad_action action selector) group_widget)
        Vector {vector_widget}->selector_monad_action_a maybe_value action this_widget environment (vector_all_selector_action DF.foldlM (selector_monad_action action selector) vector_widget)
        Widget_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Widget_io_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Widget_mix_trigger {widget}->selector_monad_action_a maybe_value action this_widget environment (selector_monad_action action selector widget)
        Coroutine {coroutine_state}->selector_monad_action_a maybe_value action this_widget environment (coroutine_all_selector_action DF.foldlM (selector_monad_action action selector) coroutine_state)
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

selector_monad_action_a::Monad h=>Maybe a->(a->Widget b c d e f->g->h g)->Widget b c d e f->g->(g->h g)->h g
selector_monad_action_a maybe_value action widget environment monad=case maybe_value of
    Nothing->monad environment
    Just value->do
        new_environment<-action value widget environment
        monad new_environment

selector_monad_action_b::Applicative h=>Bool->Maybe a->(a->Widget b c d e f->g->h g)->Widget b c d e f->g->h g
selector_monad_action_b strict maybe_value action widget environment=if strict then EE.empty_error else case maybe_value of
    Nothing->pure environment
    Just value->action value widget environment

all_selector_monad_action::Monad g=>(Widget a b c d e->f->g f)->Widget a b c d e->f->g f
all_selector_monad_action action this_widget environment=case this_widget of
    Group {group_widget}->group_all_selector_action DF.foldlM (all_selector_monad_action action) group_widget environment
    Vector {vector_widget}->vector_all_selector_action DF.foldlM (all_selector_monad_action action) vector_widget environment
    Widget_trigger {widget}->widget_trigger_selector_action (all_selector_monad_action action) widget environment
    Widget_io_trigger {widget}->widget_trigger_selector_action (all_selector_monad_action action) widget environment
    Widget_mix_trigger {widget}->widget_trigger_selector_action (all_selector_monad_action action) widget environment
    Coroutine {coroutine_state}->coroutine_all_selector_action DF.foldlM (all_selector_monad_action action) coroutine_state environment
    _->action this_widget environment

trigger_selector_monad_action::Monad g=>Bool->(Widget a b c d e->f->g f)->Widget a b c d e->f->g f
trigger_selector_monad_action bounded action this_widget environment=case this_widget of
    Group {index,group_widget}->group_default_selector_action bounded return (trigger_selector_monad_action bounded action) index group_widget environment
    Vector {index,vector_widget}->vector_default_selector_action bounded return (trigger_selector_monad_action bounded action) index vector_widget environment
    Coroutine {index,coroutine_state}->coroutine_default_selector_action bounded return (trigger_selector_monad_action bounded action) index coroutine_state environment
    _->action this_widget environment

default_selector_monad_action::Monad g=>Bool->(Widget a b c d e->f->g f)->Widget a b c d e->f->g f
default_selector_monad_action bounded action this_widget environment=case this_widget of
    Group {index,group_widget}->group_default_selector_action bounded return (default_selector_monad_action bounded action) index group_widget environment
    Vector {index,vector_widget}->vector_default_selector_action bounded return (default_selector_monad_action bounded action) index vector_widget environment
    Widget_trigger {widget}->widget_trigger_selector_action (default_selector_monad_action bounded action) widget environment
    Widget_io_trigger {widget}->widget_trigger_selector_action (default_selector_monad_action bounded action) widget environment
    Widget_mix_trigger {widget}->widget_trigger_selector_action (default_selector_monad_action bounded action) widget environment
    Coroutine {index,coroutine_state}->coroutine_default_selector_action bounded return (default_selector_monad_action bounded action) index coroutine_state environment
    _->action this_widget environment

selector_update::(a->Widget b c d e f->Widget b c d e f)->Selector a->Widget b c d e f->Widget b c d e f
selector_update update this_selector this_widget=case this_selector of
    None_selector->this_widget
    Combine_selector {combine_selector}->DF.foldl' (flip (selector_update update)) this_widget combine_selector
    Self_selector {value}->update value this_widget
    All_selector {maybe_value,value}->selector_update_a maybe_value update (all_selector_update (update value) this_widget)
    Trigger_selector {maybe_value,value,bounded}->selector_update_a maybe_value update (trigger_selector_update bounded (update value) this_widget)
    Default_selector {maybe_value,value,bounded}->selector_update_a maybe_value update (default_selector_update bounded (update value) this_widget)
    Hosted_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (group_default_selector_update bounded this_widget id (\this_index this_this_selector this_group_widget->(if bounded then int_map_update else int_map_update_safe) this_index (selector_update update this_this_selector) this_group_widget) selector initial_min_index min_index initial_max_index max_index index group_widget)
        Vector {index,vector_widget}->selector_update_a maybe_value update (vector_default_selector_update bounded this_widget id (\this_index this_this_selector this_vector_widget->CMST.runST (action_vector_widget (\this_this_vector_widget->DVM.write this_this_vector_widget this_index (selector_update update this_this_selector (this_vector_widget DV.! this_index))) this_vector_widget)) selector index vector_widget)
        Widget_trigger {next,widget_trigger,widget}->selector_update_a maybe_value update (widget_trigger_selector_update id id (selector_update update selector) next widget_trigger widget)
        Widget_io_trigger {next,widget_io_trigger,widget}->selector_update_a maybe_value update (widget_io_trigger_selector_update id id (selector_update update selector) next widget_io_trigger widget)
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->selector_update_a maybe_value update (widget_mix_trigger_selector_update id id (selector_update update selector) next widget_mix_trigger order widget)
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (coroutine_default_selector_update bounded this_widget id (\this_index this_this_selector this_coroutine_state->(if bounded then int_map_update else int_map_update_safe) this_index (update_coroutine_state (selector_update update this_this_selector)) this_coroutine_state) selector index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state)
        _->selector_update_b strict maybe_value update this_widget
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (group_all_selector_update id fmap (selector_update update selector) initial_min_index min_index initial_max_index max_index index group_widget)
        Vector {index,vector_widget}->selector_update_a maybe_value update (vector_all_selector_update id fmap (selector_update update selector) index vector_widget)
        Widget_trigger {next,widget_trigger,widget}->selector_update_a maybe_value update (widget_trigger_selector_update id id (selector_update update selector) next widget_trigger widget)
        Widget_io_trigger {next,widget_io_trigger,widget}->selector_update_a maybe_value update (widget_io_trigger_selector_update id id (selector_update update selector) next widget_io_trigger widget)
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->selector_update_a maybe_value update (widget_mix_trigger_selector_update id id (selector_update update selector) next widget_mix_trigger order widget)
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (coroutine_all_selector_update id fmap (update_coroutine_state (selector_update update selector)) index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state)
        _->selector_update_b strict maybe_value update this_widget
    Group_selector {maybe_value,group_selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_update_a maybe_value update (Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=DIM.foldlWithKey' (\this_group_widget this_index single_selector->(if bounded then int_map_update else int_map_update_safe) this_index (selector_update update single_selector) this_group_widget) group_widget group_selector})
        _->selector_update_b strict maybe_value update this_widget
    Vector_selector {maybe_value,vector_selector,bounded,strict}->case this_widget of
        Vector {index,vector_widget}->selector_update_a maybe_value update (Vector {index=index,vector_widget=CMST.runST (action_vector_widget (\this_vector_widget->CM.void (DIM.traverseWithKey (\this_index single_selector->if bounded then DVM.write this_vector_widget this_index (selector_update update single_selector (vector_widget DV.! this_index)) else maybe (return ()) (DVM.write this_vector_widget this_index . selector_update update single_selector) (vector_widget DV.!? this_index)) vector_selector)) vector_widget)})
        _->selector_update_b strict maybe_value update this_widget
    Widget_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_trigger {next,widget_trigger,widget}->selector_update_a maybe_value update (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=selector_update update selector widget})
        _->selector_update_b strict maybe_value update this_widget
    Widget_io_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_io_trigger {next,widget_io_trigger,widget}->selector_update_a maybe_value update (Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=selector_update update selector widget})
        _->selector_update_b strict maybe_value update this_widget
    Widget_mix_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->selector_update_a maybe_value update (Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=selector_update update selector widget})
        _->selector_update_b strict maybe_value update this_widget
    Coroutine_selector {maybe_value,coroutine_selector,bounded,strict}->case this_widget of
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_update_a maybe_value update (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=DIM.foldlWithKey' (\this_coroutine_state this_index single_selector->(if bounded then int_map_update else int_map_update_safe) this_index (update_coroutine_state (selector_update update single_selector)) this_coroutine_state) coroutine_state coroutine_selector,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
        _->selector_update_b strict maybe_value update this_widget

selector_update_a::Maybe a->(a->Widget b c d e f->Widget b c d e f)->Widget b c d e f->Widget b c d e f
selector_update_a maybe_value update widget=case maybe_value of
    Nothing->widget
    Just value->update value widget

selector_update_b::Bool->Maybe a->(a->Widget b c d e f->Widget b c d e f)->Widget b c d e f->Widget b c d e f
selector_update_b strict maybe_value update widget=if strict then EE.empty_error else case maybe_value of
    Nothing->widget
    Just value->update value widget

all_selector_update::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
all_selector_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->group_all_selector_update id fmap (all_selector_update update) initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->vector_all_selector_update id fmap (all_selector_update update) index vector_widget
    Widget_trigger {next,widget_trigger,widget}->widget_trigger_selector_update id id (all_selector_update update) next widget_trigger widget
    Widget_io_trigger {next,widget_io_trigger,widget}->widget_io_trigger_selector_update id id (all_selector_update update) next widget_io_trigger widget
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->widget_mix_trigger_selector_update id id (all_selector_update update) next widget_mix_trigger order widget
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->coroutine_all_selector_update id fmap (update_coroutine_state (all_selector_update update)) index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

trigger_selector_update::Bool->(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
trigger_selector_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->group_default_selector_update bounded this_widget id (\this_index this_update this_group_widget->(if bounded then int_map_update else int_map_update_safe) this_index (trigger_selector_update bounded this_update) this_group_widget) update initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->vector_default_selector_update bounded this_widget id (\this_index this_update this_vector_widget->CMST.runST (action_vector_widget (\this_this_vector_widget->DVM.write this_this_vector_widget this_index (trigger_selector_update bounded this_update (this_vector_widget DV.! this_index))) this_vector_widget)) update index vector_widget
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->coroutine_default_selector_update bounded this_widget id (\this_index this_update this_coroutine_state->(if bounded then int_map_update else int_map_update_safe) this_index (update_coroutine_state (trigger_selector_update bounded this_update)) this_coroutine_state) update index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

default_selector_update::Bool->(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
default_selector_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->group_default_selector_update bounded this_widget id (\this_index this_update this_group_widget->(if bounded then int_map_update else int_map_update_safe) this_index (default_selector_update bounded this_update) this_group_widget) update initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->vector_default_selector_update bounded this_widget id (\this_index this_update this_vector_widget->CMST.runST (action_vector_widget (\this_this_vector_widget->DVM.write this_this_vector_widget this_index (default_selector_update bounded this_update (this_vector_widget DV.! this_index))) this_vector_widget)) update index vector_widget
    Widget_trigger {next,widget_trigger,widget}->widget_trigger_selector_update id id (default_selector_update bounded update) next widget_trigger widget
    Widget_io_trigger {next,widget_io_trigger,widget}->widget_io_trigger_selector_update id id (default_selector_update bounded update) next widget_io_trigger widget
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->widget_mix_trigger_selector_update id id (default_selector_update bounded update) next widget_mix_trigger order widget
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->coroutine_default_selector_update bounded this_widget id (\this_index this_update this_coroutine_state->(if bounded then int_map_update else int_map_update_safe) this_index (update_coroutine_state (default_selector_update bounded this_update)) this_coroutine_state) update index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

selector_monad_update::Monad g=>(a->Widget b c d e f->g (Widget b c d e f))->Selector a->Widget b c d e f->g (Widget b c d e f)
selector_monad_update update this_selector this_widget=case this_selector of
    None_selector->return this_widget
    Combine_selector {combine_selector}->DF.foldlM (flip (selector_monad_update update)) this_widget combine_selector
    Self_selector {value}->update value this_widget
    All_selector {maybe_value,value}->selector_monad_update_a maybe_value update (all_selector_applicative_update (update value) this_widget)
    Trigger_selector {maybe_value,value,bounded}->selector_monad_update_a maybe_value update (trigger_selector_applicative_update bounded (update value) this_widget)
    Default_selector {maybe_value,value,bounded}->selector_monad_update_a maybe_value update (default_selector_applicative_update bounded (update value) this_widget)
    Hosted_selector {maybe_value,selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_monad_update_a maybe_value update (group_default_selector_update bounded (return this_widget) fmap (\this_index this_this_selector this_group_widget->int_map_functor_update this_index (selector_monad_update update this_this_selector) this_group_widget) selector initial_min_index min_index initial_max_index max_index index group_widget)
        Vector {index,vector_widget}->selector_monad_update_a maybe_value update (vector_default_selector_update bounded (return this_widget) fmap (\this_index this_this_selector this_vector_widget->fmap (\widget->CMST.runST (action_vector_widget (\this_this_vector_widget->DVM.write this_this_vector_widget this_index widget) this_vector_widget)) (selector_monad_update update this_this_selector (this_vector_widget DV.! this_index))) selector index vector_widget)
        Widget_trigger {next,widget_trigger,widget}->selector_monad_update_a maybe_value update (widget_trigger_selector_update fmap id (selector_monad_update update selector) next widget_trigger widget)
        Widget_io_trigger {next,widget_io_trigger,widget}->selector_monad_update_a maybe_value update (widget_io_trigger_selector_update fmap id (selector_monad_update update selector) next widget_io_trigger widget)
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->selector_monad_update_a maybe_value update (widget_mix_trigger_selector_update fmap id (selector_monad_update update selector) next widget_mix_trigger order widget)
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_monad_update_a maybe_value update (coroutine_default_selector_update bounded (return this_widget) fmap (\this_index this_this_selector this_coroutine_state->int_map_functor_update this_index (functor_update_coroutine_state (selector_monad_update update this_this_selector)) this_coroutine_state) selector index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state)
        _->selector_monad_update_b strict maybe_value update this_widget
    Any_selector {maybe_value,selector,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_monad_update_a maybe_value update (group_all_selector_update fmap traverse (selector_monad_update update selector) initial_min_index min_index initial_max_index max_index index group_widget)
        Vector {index,vector_widget}->selector_monad_update_a maybe_value update (vector_all_selector_update fmap traverse (selector_monad_update update selector) index vector_widget)
        Widget_trigger {next,widget_trigger,widget}->selector_monad_update_a maybe_value update (widget_trigger_selector_update fmap id (selector_monad_update update selector) next widget_trigger widget)
        Widget_io_trigger {next,widget_io_trigger,widget}->selector_monad_update_a maybe_value update (widget_io_trigger_selector_update fmap id (selector_monad_update update selector) next widget_io_trigger widget)
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->selector_monad_update_a maybe_value update (widget_mix_trigger_selector_update fmap id (selector_monad_update update selector) next widget_mix_trigger order widget)
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_monad_update_a maybe_value update (coroutine_all_selector_update fmap traverse (functor_update_coroutine_state (selector_monad_update update selector)) index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state)
        _->selector_monad_update_b strict maybe_value update this_widget
    Group_selector {maybe_value,group_selector,bounded,strict}->case this_widget of
        Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->selector_monad_update_c maybe_value update (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) ((if bounded then int_map_applicative_update else int_map_applicative_update_safe) (selector_monad_update update) group_selector group_widget)
        _->selector_monad_update_b strict maybe_value update this_widget
    Vector_selector {maybe_value,vector_selector,bounded,strict}->case this_widget of
        Vector {index,vector_widget}->selector_monad_update_c maybe_value update (\this_vector_widget->Vector {index=index,vector_widget=CMST.runST (action_vector_widget (\this_this_vector_widget->CM.void (DIM.traverseWithKey (maybe (return ()) . DVM.write this_this_vector_widget) this_vector_widget)) vector_widget)}) (DIM.traverseWithKey (\this_index single_selector->if bounded then fmap Just (selector_monad_update update single_selector (vector_widget DV.! this_index)) else maybe (return Nothing) (fmap Just . selector_monad_update update single_selector) (vector_widget DV.!? this_index)) vector_selector)
        _->selector_monad_update_b strict maybe_value update this_widget
    Widget_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_trigger {next,widget_trigger,widget}->selector_monad_update_c maybe_value update (\this_this_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=this_this_widget}) (selector_monad_update update selector widget)
        _->selector_monad_update_b strict maybe_value update this_widget
    Widget_io_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_io_trigger {next,widget_io_trigger,widget}->selector_monad_update_c maybe_value update (\this_this_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=this_this_widget}) (selector_monad_update update selector widget)
        _->selector_monad_update_b strict maybe_value update this_widget
    Widget_mix_trigger_selector {maybe_value,selector,strict}->case this_widget of
        Widget_mix_trigger {next,widget_mix_trigger,order,widget}->selector_monad_update_c maybe_value update (\this_this_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=this_this_widget}) (selector_monad_update update selector widget)
        _->selector_monad_update_b strict maybe_value update this_widget
    Coroutine_selector {maybe_value,coroutine_selector,bounded,strict}->case this_widget of
        Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->selector_monad_update_c maybe_value update (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) ((if bounded then int_map_applicative_update else int_map_applicative_update_safe) (functor_update_coroutine_state . selector_monad_update update) coroutine_selector coroutine_state)
        _->selector_monad_update_b strict maybe_value update this_widget

selector_monad_update_a::Monad g=>Maybe a->(a->Widget b c d e f->g (Widget b c d e f))->g (Widget b c d e f)->g (Widget b c d e f)
selector_monad_update_a maybe_value update applicative_widget=case maybe_value of
    Nothing->applicative_widget
    Just value->do
        widget<-applicative_widget
        update value widget

selector_monad_update_b::Applicative g=>Bool->Maybe a->(a->Widget b c d e f->g (Widget b c d e f))->Widget b c d e f->g (Widget b c d e f)
selector_monad_update_b strict maybe_value update widget=if strict then EE.empty_error else case maybe_value of
    Nothing->pure widget
    Just value->update value widget

selector_monad_update_c::Monad g=>Maybe a->(a->Widget b c d e f->g (Widget b c d e f))->(h->Widget b c d e f)->g h->g (Widget b c d e f)
selector_monad_update_c maybe_value update function applicative_value=case maybe_value of
    Nothing->fmap function applicative_value
    Just value->do
        new_value<-applicative_value
        update value (function new_value)

all_selector_applicative_update::Applicative f=>(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
all_selector_applicative_update update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->group_all_selector_update fmap traverse (all_selector_applicative_update update) initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->vector_all_selector_update fmap traverse (all_selector_applicative_update update) index vector_widget
    Widget_trigger {next,widget_trigger,widget}->widget_trigger_selector_update fmap id (all_selector_applicative_update update) next widget_trigger widget
    Widget_io_trigger {next,widget_io_trigger,widget}->widget_io_trigger_selector_update fmap id (all_selector_applicative_update update) next widget_io_trigger widget
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->widget_mix_trigger_selector_update fmap id (all_selector_applicative_update update) next widget_mix_trigger order widget
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->coroutine_all_selector_update fmap traverse (functor_update_coroutine_state (all_selector_applicative_update update)) index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

trigger_selector_applicative_update::Applicative f=>Bool->(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
trigger_selector_applicative_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->group_default_selector_update bounded (pure this_widget) fmap (\this_index this_update this_group_widget->int_map_functor_update this_index (trigger_selector_applicative_update bounded this_update) this_group_widget) update initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->vector_default_selector_update bounded (pure this_widget) fmap (\this_index this_update this_vector_widget->fmap (\widget->CMST.runST (action_vector_widget (\this_this_vector_widget->DVM.write this_this_vector_widget this_index widget) this_vector_widget)) (trigger_selector_applicative_update bounded this_update (this_vector_widget DV.! this_index))) update index vector_widget
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->coroutine_default_selector_update bounded (pure this_widget) fmap (\this_index this_update this_coroutine_state->int_map_functor_update this_index (functor_update_coroutine_state (trigger_selector_applicative_update bounded this_update)) this_coroutine_state) update index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

default_selector_applicative_update::Applicative f=>Bool->(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
default_selector_applicative_update bounded update this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->group_default_selector_update bounded (pure this_widget) fmap (\this_index this_update this_group_widget->int_map_functor_update this_index (default_selector_applicative_update bounded this_update) this_group_widget) update initial_min_index min_index initial_max_index max_index index group_widget
    Vector {index,vector_widget}->vector_default_selector_update bounded (pure this_widget) fmap (\this_index this_update this_vector_widget->fmap (\widget->CMST.runST (action_vector_widget (\this_this_vector_widget->DVM.write this_this_vector_widget this_index widget) this_vector_widget)) (default_selector_applicative_update bounded this_update (this_vector_widget DV.! this_index))) update index vector_widget
    Widget_trigger {next,widget_trigger,widget}->widget_trigger_selector_update fmap id (default_selector_applicative_update bounded update) next widget_trigger widget
    Widget_io_trigger {next,widget_io_trigger,widget}->widget_io_trigger_selector_update fmap id (default_selector_applicative_update bounded update) next widget_io_trigger widget
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->widget_mix_trigger_selector_update fmap id (default_selector_applicative_update bounded update) next widget_mix_trigger order widget
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->coroutine_default_selector_update bounded (pure this_widget) fmap (\this_index this_update this_coroutine_state->int_map_functor_update this_index (functor_update_coroutine_state (default_selector_applicative_update bounded this_update)) this_coroutine_state) update index initial_min_index min_index initial_max_index max_index variable_size user_variable_size layout linear_coroutine iterative coroutine_state
    _->update this_widget

update_coroutine_state::(Widget a b c d e->Widget a b c d e)->Coroutine_state a b c d e->Coroutine_state a b c d e
update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->Coroutine_state {widget=update widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}

functor_update_coroutine_state::Functor f=>(Widget a b c d e->f (Widget a b c d e))->Coroutine_state a b c d e->f (Coroutine_state a b c d e)
functor_update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->fmap (\this_widget->Coroutine_state {widget=this_widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}) (update widget)

{-# INLINE group_all_selector_action #-}
{-# INLINE group_all_selector_update #-}
{-# INLINE group_default_selector_action #-}
{-# INLINE group_default_selector_update #-}
{-# INLINE vector_all_selector_action #-}
{-# INLINE vector_all_selector_update #-}
{-# INLINE vector_default_selector_action #-}
{-# INLINE vector_default_selector_update #-}
{-# INLINE widget_trigger_selector_action #-}
{-# INLINE widget_trigger_selector_update #-}
{-# INLINE widget_io_trigger_selector_update #-}
{-# INLINE widget_mix_trigger_selector_update #-}
{-# INLINE coroutine_all_selector_action #-}
{-# INLINE coroutine_all_selector_update #-}
{-# INLINE coroutine_default_selector_action #-}
{-# INLINE coroutine_default_selector_update #-}
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
{-# INLINE update_coroutine_state #-}
{-# INLINE functor_update_coroutine_state #-}