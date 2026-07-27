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
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.C.String as FCS
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

lookup_widget::Widget a b c d e->Widget a b c d e
lookup_widget this_widget=case this_widget of
    Double {which,first_widget,second_widget}->if which then lookup_widget first_widget else lookup_widget second_widget
    Group {index,group_widget}->lookup_widget (intmap_lookup index group_widget)
    Widget_trigger {widget}->lookup_widget widget
    Widget_io_trigger {widget}->lookup_widget widget
    Widget_mix_trigger {widget}->lookup_widget widget
    Coroutine {index,coroutine_state}->lookup_widget (intmap_lookup index coroutine_state).widget
    _->this_widget

update_widget::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
update_widget update this_widget=case this_widget of
    Double {which,first_widget,second_widget}->if which then Double {which=which,first_widget=update_widget update first_widget,second_widget=second_widget} else Double {which=which,first_widget=first_widget,second_widget=update_widget update second_widget}
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=intmap_update index (update_widget update) group_widget}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_widget update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=update_widget update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=update_widget update widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=intmap_update index (update_coroutine_state (update_widget update)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

limited_update_widget::Int->(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
limited_update_widget depth update this_widget=if depth<=0 then update this_widget else case this_widget of
    Double {which,first_widget,second_widget}->if which then Double {which=which,first_widget=limited_update_widget (depth-1) update first_widget,second_widget=second_widget} else Double {which=which,first_widget=first_widget,second_widget=limited_update_widget (depth-1) update second_widget}
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=intmap_update index (limited_update_widget (depth-1) update) group_widget}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=limited_update_widget (depth-1) update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=limited_update_widget (depth-1) update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=limited_update_widget (depth-1) update widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=intmap_update index (update_coroutine_state (limited_update_widget (depth-1) update)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

functor_update_widget::Functor f=>(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
functor_update_widget update this_widget=case this_widget of
    Double {which,first_widget,second_widget}->if which then fmap (\this_first_widget->Double {which=which,first_widget=this_first_widget,second_widget=second_widget}) (functor_update_widget update first_widget) else fmap (\this_second_widget->Double {which=which,first_widget=first_widget,second_widget=this_second_widget}) (functor_update_widget update second_widget)
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (functor_update_widget update) group_widget)
    Widget_trigger {next,widget_trigger,widget}->fmap (\single_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=single_widget}) (functor_update_widget update widget)
    Widget_io_trigger {next,widget_io_trigger,widget}->fmap (\single_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=single_widget}) (functor_update_widget update widget)
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->fmap (\single_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=single_widget}) (functor_update_widget update widget)
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (functor_update_widget update)) coroutine_state)
    _->update this_widget

functor_limited_update_widget::Functor f=>Int->(Widget a b c d e->f (Widget a b c d e))->Widget a b c d e->f (Widget a b c d e)
functor_limited_update_widget depth update this_widget=if depth<=0 then update this_widget else case this_widget of
    Double {which,first_widget,second_widget}->if which then fmap (\this_first_widget->Double {which=which,first_widget=this_first_widget,second_widget=second_widget}) (functor_limited_update_widget (depth-1) update first_widget) else fmap (\this_second_widget->Double {which=which,first_widget=first_widget,second_widget=this_second_widget}) (functor_limited_update_widget (depth-1) update second_widget)
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->fmap (\this_group_widget->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=this_group_widget}) (intmap_functor_update index (functor_limited_update_widget (depth-1) update) group_widget)
    Widget_trigger {next,widget_trigger,widget}->fmap (\single_widget->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=single_widget}) (functor_limited_update_widget (depth-1) update widget)
    Widget_io_trigger {next,widget_io_trigger,widget}->fmap (\single_widget->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=single_widget}) (functor_limited_update_widget (depth-1) update widget)
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->fmap (\single_widget->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=single_widget}) (functor_limited_update_widget (depth-1) update widget)
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->fmap (\this_coroutine_state->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=this_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}) (intmap_functor_update index (functor_update_coroutine_state (functor_limited_update_widget (depth-1) update)) coroutine_state)
    _->update this_widget

update_all_widget::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
update_all_widget update this_widget=case this_widget of
    Double {which,first_widget,second_widget}->Double {which=which,first_widget=update_all_widget update first_widget,second_widget=update_all_widget update second_widget}
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=fmap (update_all_widget update) group_widget}
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_all_widget update widget}
    Widget_io_trigger {next,widget_io_trigger,widget}->Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=update_all_widget update widget}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=update_all_widget update widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=fmap (update_coroutine_state (update_all_widget update)) coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->update this_widget

update_coroutine_state::(Widget a b c d e->Widget a b c d e)->Coroutine_state a b c d e->Coroutine_state a b c d e
update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->Coroutine_state {widget=update widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}

functor_update_coroutine_state::Functor f=>(Widget a b c d e->f (Widget a b c d e))->Coroutine_state a b c d e->f (Coroutine_state a b c d e)
functor_update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->fmap (\this_widget->Coroutine_state {widget=this_widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}) (update widget)

seq_poke_array::FS.Storable a=>Int->DS.Seq a->FP.Ptr a->IO ()
seq_poke_array size value ptr=CM.void (DF.foldlM (\this_ptr single_value->FS.poke this_ptr single_value>>return (FP.plusPtr this_ptr size)) ptr value)

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
has_clipboard_text=FMU.toBool <$> SDLF.sdl_has_clipboard_text

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