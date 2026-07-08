{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Helper where

import Engine.Container
import Engine.Type
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

catch_false::IO FCT.CBool->IO ()
catch_false io=do
    value<-io
    CM.unless (FMU.toBool value) (error "catch_false: error 1")

catch_zero::(Eq a,Num a)=>a->IO ()
catch_zero number=case number of
    0->error "catch_zero: error 1"
    _->return ()

catch_null::FP.Ptr a->IO ()
catch_null ptr=CM.when (ptr==FP.nullPtr) (error "catch_null: error 1")

return_catch_null::IO (FP.Ptr a)->IO (FP.Ptr a)
return_catch_null io=do
    ptr<-io
    if ptr==FP.nullPtr then error "return_catch_null: error 1" else return ptr

lookup_widget::Widget a->Widget a
lookup_widget this_widget=case this_widget of
    Double {which,first_widget,second_widget}->if which then lookup_widget first_widget else lookup_widget second_widget
    Group {index,group_widget}->lookup_widget (intmap_lookup index group_widget)
    Widget_trigger {widget}->lookup_widget widget
    Widget_io_trigger {widget}->lookup_widget widget
    Widget_mix_trigger {widget}->lookup_widget widget
    Coroutine {index,coroutine_state}->lookup_widget (intmap_lookup index coroutine_state).widget
    _->this_widget

update_widget::(Widget a->Widget a)->Widget a->Widget a
update_widget update this_widget=case this_widget of
    Double {which,first_widget,second_widget}->if which then Double {which=which,first_widget=update_widget update first_widget,second_widget=second_widget} else Double {which=which,first_widget=first_widget,second_widget=update_widget update second_widget}
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=intmap_update index (update_widget update) group_widget}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_widget update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=update_widget update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=update_widget update widget}
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,variable_length,index,coroutine_state,linear_coroutine,iterative}->Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,index=index,coroutine_state=intmap_update index (update_coroutine_state (update_widget update)) coroutine_state,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

limited_update_widget::Int->(Widget a->Widget a)->Widget a->Widget a
limited_update_widget depth update this_widget=if depth<=0 then update this_widget else case this_widget of
    Double {which,first_widget,second_widget}->if which then Double {which=which,first_widget=limited_update_widget (depth-1) update first_widget,second_widget=second_widget} else Double {which=which,first_widget=first_widget,second_widget=limited_update_widget (depth-1) update second_widget}
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=intmap_update index (limited_update_widget (depth-1) update) group_widget}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=limited_update_widget (depth-1) update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=limited_update_widget (depth-1) update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=limited_update_widget (depth-1) update widget}
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,variable_length,index,coroutine_state,linear_coroutine,iterative}->Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,index=index,coroutine_state=intmap_update index (update_coroutine_state (limited_update_widget (depth-1) update)) coroutine_state,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

functor_update_widget::Functor b=>(Widget a->b (Widget a))->Widget a->b (Widget a)
functor_update_widget update this_widget=case this_widget of
    Double {which,first_widget,second_widget}->if which then fmap (\this_first_widget->Double {which=which,first_widget=this_first_widget,second_widget=second_widget}) (functor_update_widget update first_widget) else fmap (\this_second_widget->Double {which=which,first_widget=first_widget,second_widget=this_second_widget}) (functor_update_widget update second_widget)
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (functor_update_widget update) group_widget)
    Widget_trigger {next,widget_trigger,widget}->fmap (\this_this_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=this_this_widget}) (functor_update_widget update widget)
    Widget_io_trigger {next,widget_io_trigger,widget}->fmap (\this_this_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=this_this_widget}) (functor_update_widget update widget)
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->fmap (\this_this_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=this_this_widget}) (functor_update_widget update widget)
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,variable_length,index,coroutine_state,linear_coroutine,iterative}->fmap (\this_coroutine_state->Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,index=index,coroutine_state=this_coroutine_state,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (functor_update_widget update)) coroutine_state)
    _->update this_widget

functor_limited_update_widget::Functor b=>Int->(Widget a->b (Widget a))->Widget a->b (Widget a)
functor_limited_update_widget depth update this_widget=if depth<=0 then update this_widget else case this_widget of
    Double {which,first_widget,second_widget}->if which then fmap (\this_first_widget->Double {which=which,first_widget=this_first_widget,second_widget=second_widget}) (functor_limited_update_widget (depth-1) update first_widget) else fmap (\this_second_widget->Double {which=which,first_widget=first_widget,second_widget=this_second_widget}) (functor_limited_update_widget (depth-1) update second_widget)
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (functor_limited_update_widget (depth-1) update) group_widget)
    Widget_trigger {next,widget_trigger,widget}->fmap (\this_this_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=this_this_widget}) (functor_limited_update_widget (depth-1) update widget)
    Widget_io_trigger {next,widget_io_trigger,widget}->fmap (\this_this_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=this_this_widget}) (functor_limited_update_widget (depth-1) update widget)
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->fmap (\this_this_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=this_this_widget}) (functor_limited_update_widget (depth-1) update widget)
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,variable_length,index,coroutine_state,linear_coroutine,iterative}->fmap (\this_coroutine_state->Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,index=index,coroutine_state=this_coroutine_state,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (functor_limited_update_widget (depth-1) update)) coroutine_state)
    _->update this_widget

update_all_widget::(Widget a->Widget a)->Widget a->Widget a
update_all_widget update this_widget=case this_widget of
    Double {which,first_widget,second_widget}->Double {which=which,first_widget=update_all_widget update first_widget,second_widget=update_all_widget update second_widget}
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=fmap (update_all_widget update) group_widget}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_all_widget update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=update_all_widget update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=update_all_widget update widget}
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,variable_length,index,coroutine_state,linear_coroutine,iterative}->Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,index=index,coroutine_state=fmap (update_coroutine_state (update_all_widget update)) coroutine_state,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

update_coroutine_state::(Widget a->Widget a)->Coroutine_state a->Coroutine_state a
update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->Coroutine_state {widget=update widget,variable=variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}

functor_update_coroutine_state::Functor b=>(Widget a->b (Widget a))->Coroutine_state a->b (Coroutine_state a)
functor_update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->fmap (\this_widget->Coroutine_state {widget=this_widget,variable=variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}) (update widget)

seq_poke_array::FS.Storable a=>Int->DS.Seq a->FP.Ptr a->IO ()
seq_poke_array size value ptr=CM.void (DF.foldlM (\this_ptr single_value->FS.poke this_ptr single_value>>return (FP.plusPtr this_ptr size)) ptr value)

point_addition::Point->Point->Point
point_addition first_point second_point=case first_point of
    Point {x=first_x,y=first_y}->case second_point of
        Point {x=second_x,y=second_y}->Point {x=first_x+second_x,y=first_y+second_y}

move_matrix::Point->Matrix->Matrix
move_matrix point matrix=case matrix of
    Matrix {x,y,x_x,x_y,y_x,y_y}->Matrix {x=point.x+x,y=point.y+y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y}

identity_matrix::Matrix
identity_matrix=Matrix {x=0,y=0,x_x=1,x_y=0,y_x=0,y_y=1}

to_extended::a->Extended a
to_extended number=Finite {number=number}

from_extended::Num a=>Extended a->a
from_extended extended=case extended of
    Negative_infinity->0
    Finite {number}->number
    Positive_infinity->0

mebibyte::Num a=>a
mebibyte=1048576

nanosecond::Num a=>a
nanosecond=1000000000