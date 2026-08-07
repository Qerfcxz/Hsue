{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Operation where

import Engine.Container
import Engine.Type
import qualified Error.Error as EE
import qualified Control.Monad.ST as CMST
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM

get_store_widget::(Convert Data f)=>Widget a b c d e->f
get_store_widget widget=case widget of
    Store {store}->convert store
    _->EE.quick_error "get_store_widget" 0

update_store_widget::(Convert Data a,Convert a Data)=>(a->a)->Widget b c d e f->Widget b c d e f
update_store_widget update widget=case widget of
    Store {store}->Store {store=convert (update (convert store))}
    _->EE.quick_error "update_store_widget" 0

update_vector_widget::Int->(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
update_vector_widget this_index update widget=case widget of
    Vector {index,size,vector_widget}->Vector {index=index,size=size,vector_widget=CMST.runST (transform_vector_widget (\this_vector_widget->DVM.write this_vector_widget this_index (update (vector_widget DV.! this_index))) vector_widget)}
    _->EE.quick_error "update_vector_widget" 0

default_update_vector_widget::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
default_update_vector_widget update widget=case widget of
    Vector {index,size,vector_widget}->Vector {index=index,size=size,vector_widget=CMST.runST (transform_vector_widget (\this_vector_widget->DVM.write this_vector_widget index (update (vector_widget DV.! index))) vector_widget)}
    _->EE.quick_error "default_update_vector_widget" 0

transform_vector_widget::DVM.PrimMonad a=>(DVM.MVector (DVM.PrimState a) (Widget b c d e f)->a ())->DV.Vector (Widget b c d e f)->a (DV.Vector (Widget b c d e f))
transform_vector_widget function vector_widget=do
    new_vector_widget<-DV.thaw vector_widget
    function new_vector_widget
    DV.unsafeFreeze new_vector_widget

update_group_widget::Int->(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
update_group_widget this_index update widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=intmap_update this_index update group_widget}
    _->EE.quick_error "update_group_widget" 0

default_update_group_widget::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
default_update_group_widget update widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=intmap_update index update group_widget}
    _->EE.quick_error "default_update_group_widget" 0

widget_lookup::Widget a b c d e->Widget a b c d e
widget_lookup this_widget=case this_widget of
    Group {index,group_widget}->widget_lookup (intmap_lookup index group_widget)
    Vector {index,vector_widget}->widget_lookup (vector_widget DV.! index)
    Widget_trigger {widget}->widget_lookup widget
    Widget_io_trigger {widget}->widget_lookup widget
    Widget_mix_trigger {widget}->widget_lookup widget
    Coroutine {index,coroutine_state}->widget_lookup (intmap_lookup index coroutine_state).widget
    _->this_widget