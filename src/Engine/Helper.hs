{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Helper where

import Engine.Container
import Engine.Engine
import Engine.Type
import Engine.Underlying
import qualified SDL.Function as SDLF
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as DHMS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP

self_selector::Selector ()
self_selector=Self_selector {value=()}

all_selector::Bool->Selector ()
all_selector this_maybe=All_selector {maybe_value=if this_maybe then Just () else Nothing,value=()}

trigger_selector::Bool->Bool->Selector ()
trigger_selector this_maybe bounded=Trigger_selector {maybe_value=if this_maybe then Just () else Nothing,value=(),bounded=bounded}

default_selector::Bool->Bool->Selector ()
default_selector this_maybe bounded=Default_selector {maybe_value=if this_maybe then Just () else Nothing,value=(),bounded=bounded}

simple_calculate_typesetting::FCT.CFloat->FCT.CFloat->DS.Seq (DS.Seq Row)->Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat)
simple_calculate_typesetting height line_spacing article row_number=let new_row_number=DF.sum (fmap DS.length article) in if new_row_number==0||new_row_number<=row_number then (0,0,0) else let half_line_spacing=line_spacing/2 in let padding=(height-fromIntegral (new_row_number-1)*line_spacing)/2 in (if row_number==new_row_number-1 then padding else half_line_spacing,if row_number==0 then padding else half_line_spacing,0)

origin::Point
origin=Point {x=0,y=0}

fit_matrix::Engine a b c d e->Int->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Matrix
fit_matrix engine window_id widget_width widget_height width height=let window=int_map_lookup window_id engine.window in let scale=min (width/widget_width*window.adaptive_width/window.width) (height/widget_height*window.adaptive_height/window.height) in Matrix {x=0,y=0,x_x=scale,x_y=0,y_x=0,y_y=scale}

fit_window_matrix::Engine a b c d e->Int->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Matrix
fit_window_matrix engine window_id widget_width widget_height window_width_scale window_height_scale=let window=int_map_lookup window_id engine.window in let scale=min (window_width_scale*window.adaptive_width/widget_width) (window_height_scale*window.adaptive_height/widget_height) in Matrix {x=0,y=0,x_x=scale,x_y=0,y_x=0,y_y=scale}

identity_matrix::Matrix
identity_matrix=Matrix {x=0,y=0,x_x=1,x_y=0,y_x=0,y_y=1}

x_scalable_matrix::FCT.CFloat->FCT.CFloat->Matrix->Matrix
x_scalable_matrix x x_x matrix=case matrix of
    Matrix {y,x_y,y_x,y_y}->Matrix {x=x,y=y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y}

y_scalable_matrix::FCT.CFloat->FCT.CFloat->Matrix->Matrix
y_scalable_matrix y y_y matrix=case matrix of
    Matrix {x,x_x,x_y,y_x}->Matrix {x=x,y=y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y}

update_arrange_matrix::(Matrix->Matrix)->Arrange->Arrange
update_arrange_matrix update arrange=case arrange of
    Arrange {point,matrix,color}->Arrange {point=point,matrix=update matrix,color=color}

from_foldable_enumeration::(Foldable a,Enum b)=>a b->Integer
from_foldable_enumeration=DF.foldl' (\int value->int DB..|. DB.bit (fromEnum value)) 0

insert_foldable_enumeration::(Foldable a,Enum b)=>a b->c->DHMS.HashMap Integer c->DHMS.HashMap Integer c
insert_foldable_enumeration foldable=hash_map_insert (from_foldable_enumeration foldable)

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

quick_create_engine::a->(Event b->Engine a b c d e->Maybe Int)->(Event b->Engine a b c d e->Projection_strategy)->FCT.CInt->Int->Int->Int->Int->Int->Maybe DW.Word64->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->Sampler_create_info->Blend_state->IO (Engine a b c d e)
quick_create_engine state main_id projection_strategy max_picture_size max_vertex_size max_index_size max_parameter_size exponent_width exponent_height maybe_interval padding width height font_size pixel_range sampler_create_info blend_state=case maybe_interval of
    Nothing->create_engine state main_id projection_strategy (max_picture_size*mebibyte) (max_vertex_size*mebibyte) (max_index_size*mebibyte) (max_parameter_size*mebibyte) exponent_width exponent_height 0 0 0 0 Nothing 0 padding width height font_size pixel_range sampler_create_info blend_state
    Just interval->create_engine state main_id projection_strategy (max_picture_size*mebibyte) (max_vertex_size*mebibyte) (max_index_size*mebibyte) (max_parameter_size*mebibyte) exponent_width exponent_height 0 0 0 0 (Just (div nanosecond interval)) 0 padding width height font_size pixel_range sampler_create_info blend_state

{-# INLINE self_selector #-}
{-# INLINE all_selector #-}
{-# INLINE trigger_selector #-}
{-# INLINE default_selector #-}
{-# INLINE simple_calculate_typesetting #-}
{-# INLINE origin #-}
{-# INLINE fit_matrix #-}
{-# INLINE fit_window_matrix #-}
{-# INLINE identity_matrix #-}
{-# INLINE x_scalable_matrix #-}
{-# INLINE y_scalable_matrix #-}
{-# INLINE update_arrange_matrix #-}
{-# INLINE from_foldable_enumeration #-}
{-# INLINE insert_foldable_enumeration #-}
{-# INLINE get_clipboard_text #-}
{-# INLINE has_clipboard_text #-}
{-# INLINE set_clipboard_text #-}
{-# INLINE quick_create_engine #-}