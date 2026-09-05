{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Helper where

import Engine.Container
import Engine.Engine
import Engine.Request
import Engine.Type
import Engine.Underlying
import qualified SDL.Function as SDLF
import qualified Error.Type as ET
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as DHMS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Vector as DV
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP

self_selector::ET.Has_call_stack=>Selector ()
self_selector=Self_selector {value=()}

all_selector::ET.Has_call_stack=>Bool->Selector ()
all_selector maybe_value=All_selector {maybe_value=if maybe_value then Just () else Nothing,value=()}

trigger_selector::ET.Has_call_stack=>Bool->Bool->Selector ()
trigger_selector strict_exist maybe_value=Trigger_selector {maybe_value=if maybe_value then Just () else Nothing,value=(),strict_exist=strict_exist}

default_selector::ET.Has_call_stack=>Bool->Bool->Selector ()
default_selector strict_exist maybe_value=Default_selector {maybe_value=if maybe_value then Just () else Nothing,value=(),strict_exist=strict_exist}

any_visual_selector::ET.Has_call_stack=>Bool->Visual_selector ()
any_visual_selector strict_match=Any_visual_selector {value=(),strict_match=strict_match}

visual_trigger_selector::ET.Has_call_stack=>Bool->Visual_selector ()
visual_trigger_selector strict_match=Visual_trigger_selector {value=(),strict_match=strict_match}

visual_io_trigger_selector::ET.Has_call_stack=>Bool->Visual_selector ()
visual_io_trigger_selector strict_match=Visual_io_trigger_selector {value=(),strict_match=strict_match}

visual_mix_trigger_selector::ET.Has_call_stack=>Bool->Visual_selector ()
visual_mix_trigger_selector strict_match=Visual_mix_trigger_selector {value=(),strict_match=strict_match}

const_dynamic_bool::ET.Has_call_stack=>Bool->Dynamic_bool a
const_dynamic_bool bool=Dynamic_bool {dynamic_bool=const (const (const (const bool)))}

const_dynamic_int::ET.Has_call_stack=>Int->Dynamic_int a
const_dynamic_int int=Dynamic_int {dynamic_int=const (const (const (const int)))}

create_foldable_request::ET.Has_call_stack=>Foldable a=>a (Request b)->Engine b->Engine b
create_foldable_request foldable_request engine=DF.foldl' (flip create_request) engine foldable_request

consume_object_move::ET.Has_call_stack=>Int->Projection_move
consume_object_move leaf_id=Object_move {leaf_id=leaf_id,consume=True}

retain_object_move::ET.Has_call_stack=>Int->Projection_move
retain_object_move leaf_id=Object_move {leaf_id=leaf_id,consume=False}

simple_window_render_request::ET.Has_call_stack=>Int->Projection_move->Maybe Int->Request a
simple_window_render_request window_id projection_move maybe_sampler_id=Render {window_id=window_id,render_selector=Self_selector {value=()},projection_move=projection_move,maybe_sampler_id=maybe_sampler_id}

simple_canvas_render_request::ET.Has_call_stack=>Int->Projection_move->Maybe Int->Request a
simple_canvas_render_request canvas_id projection_move maybe_sampler_id=Canvas_render {canvas_id=canvas_id,canvas_render_selector=Self_selector {value=()},projection_move=projection_move,maybe_sampler_id=maybe_sampler_id}

simple_calculate_typesetting::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->DS.Seq (DS.Seq Row)->Int->Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat)
simple_calculate_typesetting height line_spacing _ number index=if number==0||number<=index then (0,0,0) else let half_line_spacing=line_spacing/2 in let padding=(height-fromIntegral (number-1)*line_spacing)/2 in (if index==number-1 then padding else half_line_spacing,if index==0 then padding else half_line_spacing,0)

simple_article::ET.Has_call_stack=>DT.Text->FCT.CFloat->Color->DT.Text->DS.Seq (DS.Seq Sentence)
simple_article text font_size color path=DS.singleton (DS.singleton (Sentence {sentence_core=DS.singleton (Phrase {phrase_core=text,font_size=font_size,color=color}),path=path}))

origin_point::ET.Has_call_stack=>Point
origin_point=Point {x=0,y=0}

plus_point::ET.Has_call_stack=>Point->Point->Point
plus_point first_point second_point=case first_point of
    Point {x=first_x,y=first_y}->case second_point of
        Point {x=second_x,y=second_y}->Point {x=first_x+second_x,y=first_y+second_y}

subtract_point::ET.Has_call_stack=>Point->Point->Point
subtract_point first_point second_point=case first_point of
    Point {x=first_x,y=first_y}->case second_point of
        Point {x=second_x,y=second_y}->Point {x=first_x-second_x,y=first_y-second_y}

identity_matrix::ET.Has_call_stack=>Matrix
identity_matrix=Matrix {x=0,y=0,x_x=1,x_y=0,y_x=0,y_y=1}

scale_matrix::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Matrix
scale_matrix x_scale y_scale=Matrix {x=0,y=0,x_x=x_scale,x_y=0,y_x=0,y_y=y_scale}

origin_matrix::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Matrix
origin_matrix x y=Matrix {x=x,y=y,x_x=1,x_y=0,y_x=0,y_y=1}

rotate_matrix::ET.Has_call_stack=>FCT.CFloat->Matrix
rotate_matrix angle=let cos_angle=cos angle in let sin_angle=sin angle in Matrix {x=0,y=0,x_x=cos_angle,x_y=negate sin_angle,y_x=sin_angle,y_y=cos_angle}

update_x_matrix::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Matrix->Matrix
update_x_matrix x x_x matrix=case matrix of
    Matrix {y,x_y,y_x,y_y}->Matrix {x=x,y=y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y}

update_y_matrix::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Matrix->Matrix
update_y_matrix y y_y matrix=case matrix of
    Matrix {x,x_x,x_y,y_x}->Matrix {x=x,y=y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y}

multiply_matrix::ET.Has_call_stack=>Matrix->Matrix->Matrix
multiply_matrix first_matrix second_matrix=case first_matrix of
    Matrix {x=first_x,y=first_y,x_x=first_x_x,x_y=first_x_y,y_x=first_y_x,y_y=first_y_y}->case second_matrix of
        Matrix {x=second_x,y=second_y,x_x=second_x_x,x_y=second_x_y,y_x=second_y_x,y_y=second_y_y}->Matrix {x=if second_x==0&&second_y==0 then first_x else second_x,y=if second_x==0&&second_y==0 then first_y else second_y,x_x=first_x_x*second_x_x+first_x_y*second_y_x,x_y=first_x_x*second_x_y+first_x_y*second_y_y,y_x=first_y_x*second_x_x+first_y_y*second_y_x,y_y=first_y_x*second_x_y+first_y_y*second_y_y}

opaque_color::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->Color
opaque_color red green blue=Color {red=red,green=green,blue=blue,alpha=1}

transparent_color::ET.Has_call_stack=>Color
transparent_color=Color {red=0,green=0,blue=0,alpha=0}

white_color::ET.Has_call_stack=>Color
white_color=Color {red=1,green=1,blue=1,alpha=1}

black_color::ET.Has_call_stack=>Color
black_color=Color {red=0,green=0,blue=0,alpha=1}

update_color_alpha::ET.Has_call_stack=>FCT.CFloat->Color->Color
update_color_alpha alpha color=case color of
    Color {red,green,blue}->Color {red=red,green=green,blue=blue,alpha=alpha}

default_arrange::ET.Has_call_stack=>Arrange
default_arrange=Arrange {point=origin_point,matrix=identity_matrix,color=white_color}

single_visual_request::ET.Has_call_stack=>Visual_request a->Widget_request a
single_visual_request visual_request=Vector_visual_request {arrange=default_arrange,vector_visual_request=DV.singleton visual_request}

fit_matrix::ET.Has_call_stack=>Engine a->Int->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Matrix
fit_matrix engine window_id widget_width widget_height width height=let window=int_map_lookup window_id engine.window in let scale=min (width/widget_width*window.adaptive_width/window.width) (height/widget_height*window.adaptive_height/window.height) in Matrix {x=0,y=0,x_x=scale,x_y=0,y_x=0,y_y=scale}

fit_window_matrix::ET.Has_call_stack=>Engine a->Int->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Matrix
fit_window_matrix engine window_id widget_width widget_height window_width_scale window_height_scale=let window=int_map_lookup window_id engine.window in let scale=min (window_width_scale*window.adaptive_width/widget_width) (window_height_scale*window.adaptive_height/widget_height) in Matrix {x=0,y=0,x_x=scale,x_y=0,y_x=0,y_y=scale}

from_foldable_enumeration::ET.Has_call_stack=>Foldable a=>Enum b=>a b->Integer
from_foldable_enumeration=DF.foldl' (\int enumeration->int DB..|. DB.bit (fromEnum enumeration)) 0

insert_foldable_enumeration::ET.Has_call_stack=>Foldable a=>Enum b=>a b->c->DHMS.HashMap Integer c->DHMS.HashMap Integer c
insert_foldable_enumeration foldable_enumeration=hash_map_insert_strict (from_foldable_enumeration foldable_enumeration)

get_clipboard_text::ET.Has_call_stack=>IO DT.Text
get_clipboard_text=do
    ptr<-SDLF.sdl_get_clipboard_text
    sdl_catch_null ptr
    string<-DBS.packCString ptr
    SDLF.sdl_free (FP.castPtr ptr)
    return (DTE.decodeUtf8 string)

has_clipboard_text::ET.Has_call_stack=>IO Bool
has_clipboard_text=fmap FMU.toBool SDLF.sdl_has_clipboard_text

set_clipboard_text::ET.Has_call_stack=>DT.Text->IO Bool
set_clipboard_text string=with_text string $ \this_string->do
    value<-SDLF.sdl_set_clipboard_text this_string
    return (FMU.toBool value)

quick_create_engine::ET.Has_call_stack=>Custom_state a->(Event a->Engine a->Maybe Int)->(Event a->Engine a->Projection_strategy)->FCT.CFloat->FCT.CFloat->FCT.CInt->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->Maybe DW.Word64->Int->Int->Sampler_create_info->Blend_state->Bool->Bool->Bool->Bool->IO (Engine a)
quick_create_engine state main_id projection_strategy font_size pixel_range max_picture_size max_vertex_size max_index_size max_parameter_size padding width height maybe_interval exponent_width exponent_height sampler_create_info blend_state strict_exist strict_match strict_resource strict_capacity=case maybe_interval of
    Nothing->create_engine state main_id projection_strategy font_size pixel_range (max_picture_size*mebibyte) (max_vertex_size*mebibyte) (max_index_size*mebibyte) (max_parameter_size*mebibyte) padding width height 0 Nothing 0 0 0 0 exponent_width exponent_height sampler_create_info blend_state strict_exist strict_match strict_resource strict_capacity
    Just interval->create_engine state main_id projection_strategy font_size pixel_range (max_picture_size*mebibyte) (max_vertex_size*mebibyte) (max_index_size*mebibyte) (max_parameter_size*mebibyte) padding width height 0 (Just (div nanosecond interval)) 0 0 0 0 exponent_width exponent_height sampler_create_info blend_state strict_exist strict_match strict_resource strict_capacity

{-# INLINE self_selector #-}
{-# INLINE all_selector #-}
{-# INLINE trigger_selector #-}
{-# INLINE default_selector #-}
{-# INLINE any_visual_selector #-}
{-# INLINE visual_trigger_selector #-}
{-# INLINE visual_io_trigger_selector #-}
{-# INLINE visual_mix_trigger_selector #-}
{-# INLINE const_dynamic_bool #-}
{-# INLINE const_dynamic_int #-}
{-# INLINE create_foldable_request #-}
{-# INLINE consume_object_move #-}
{-# INLINE retain_object_move #-}
{-# INLINE simple_window_render_request #-}
{-# INLINE simple_canvas_render_request #-}
{-# INLINE simple_calculate_typesetting #-}
{-# INLINE simple_article #-}
{-# INLINE origin_point #-}
{-# INLINE plus_point #-}
{-# INLINE subtract_point #-}
{-# INLINE identity_matrix #-}
{-# INLINE scale_matrix #-}
{-# INLINE origin_matrix #-}
{-# INLINE rotate_matrix #-}
{-# INLINE update_x_matrix #-}
{-# INLINE update_y_matrix #-}
{-# INLINE multiply_matrix #-}
{-# INLINE opaque_color #-}
{-# INLINE transparent_color #-}
{-# INLINE white_color #-}
{-# INLINE black_color #-}
{-# INLINE update_color_alpha #-}
{-# INLINE default_arrange #-}
{-# INLINE single_visual_request #-}
{-# INLINE fit_matrix #-}
{-# INLINE fit_window_matrix #-}
{-# INLINE from_foldable_enumeration #-}
{-# INLINE insert_foldable_enumeration #-}
{-# INLINE quick_create_engine #-}