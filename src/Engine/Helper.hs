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
import qualified Data.Vector as DV
import qualified Data.Word as DW
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

widget_lookup::Widget a b c d e->Widget a b c d e
widget_lookup this_widget=case this_widget of
    Group {index,group_widget}->widget_lookup (intmap_lookup index group_widget)
    Vector {index,vector_widget}->widget_lookup (vector_widget DV.! index)
    Widget_trigger {widget}->widget_lookup widget
    Widget_io_trigger {widget}->widget_lookup widget
    Widget_mix_trigger {widget}->widget_lookup widget
    Coroutine {index,coroutine_state}->widget_lookup (intmap_lookup index coroutine_state).widget
    _->this_widget

self_selector::Selector ()
self_selector=Self_selector {value=()}

all_selector::Bool->Selector ()
all_selector this_maybe=All_selector {maybe_value=if this_maybe then Just () else Nothing,value=()}

trigger_selector::Bool->Bool->Selector ()
trigger_selector this_maybe bounded=Trigger_selector {maybe_value=if this_maybe then Just () else Nothing,value=(),bounded=bounded}

default_selector::Bool->Bool->Selector ()
default_selector this_maybe bounded=Default_selector {maybe_value=if this_maybe then Just () else Nothing,value=(),bounded=bounded}

seq_poke_array::FS.Storable a=>Int->DS.Seq a->FP.Ptr a->IO ()
seq_poke_array size value ptr=CM.void (DF.foldlM (flip (seq_poke_array_a size)) ptr value)

seq_poke_array_a::FS.Storable a=>Int->a->FP.Ptr a->IO (FP.Ptr a)
seq_poke_array_a size value ptr=do
    FS.poke ptr value
    return (FP.plusPtr ptr size)

triple_reverse::(a,b,c)->(c,b,a)
triple_reverse (a,b,c)=(c,b,a)

fit_matrix::Engine a b c d e->Int->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Matrix
fit_matrix engine window_id widget_width widget_height width height=let window=intmap_lookup window_id engine.window in let scale=min (width/widget_width*window.adaptive_width/window.width) (height/widget_height*window.adaptive_height/window.height) in Matrix {x=0,y=0,x_x=scale,x_y=0,y_x=0,y_y=scale}

fit_window_matrix::Engine a b c d e->Int->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Matrix
fit_window_matrix engine window_id widget_width widget_height window_width_scale window_height_scale=let window=intmap_lookup window_id engine.window in let scale=min (window_width_scale*window.adaptive_width/widget_width) (window_height_scale*window.adaptive_height/widget_height) in Matrix {x=0,y=0,x_x=scale,x_y=0,y_x=0,y_y=scale}

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
    string<-DBS.packCString ptr
    SDLF.sdl_free (FP.castPtr ptr)
    return (DT.unpack (DTE.decodeUtf8 string))

has_clipboard_text::IO Bool
has_clipboard_text=fmap FMU.toBool SDLF.sdl_has_clipboard_text

set_clipboard_text::String->IO Bool
set_clipboard_text string=with_string string $ \ptr->do
    value<-SDLF.sdl_set_clipboard_text ptr
    return (FMU.toBool value)

quick_create_engine::(a->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Projection_strategy)->FCT.CInt->Int->Int->Int->Int->Int->Int->Int->Maybe DW.Word64->DW.Word64->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->Sampler_create_info->IO (Engine a b c d e))->a->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Projection_strategy)->FCT.CInt->Int->Int->Int->Maybe DW.Word64->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->Sampler_create_info->IO (Engine a b c d e)
quick_create_engine create_engine state main_id projection_strategy picture_size vertex_size index_size parameter_size maybe_interval padding width height font_size pixel_range sampler_create_info=case maybe_interval of
    Nothing->create_engine state main_id projection_strategy (picture_size*mebibyte) (vertex_size*mebibyte) (index_size*mebibyte) (parameter_size*mebibyte) 0 0 0 0 Nothing 0 padding width height font_size pixel_range sampler_create_info
    Just interval->create_engine state main_id projection_strategy (picture_size*mebibyte) (vertex_size*mebibyte) (index_size*mebibyte) (parameter_size*mebibyte) 0 0 0 0 (Just (div nanosecond interval)) 0 padding width height font_size pixel_range sampler_create_info

mebibyte::Num a=>a
mebibyte=1048576

nanosecond::Num a=>a
nanosecond=1000000000

millisecond::Num a=>a
millisecond=1000000