{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Helper where

import Engine.Animation
import Engine.Collector
import Engine.Container
import Engine.Coroutine
import Engine.Engine
import Engine.Projection
import Engine.Request
import Engine.Text
import Engine.Type
import Engine.Underlying
import Engine.Widget
import Engine.Window
import qualified SDL.Function as SDLF
import qualified Error.Type as ET
import qualified Data.Bits as DB
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as DHMS
import qualified Data.HashSet as DHS
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Vector as DV
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP

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

simple_window_render_request::ET.Has_call_stack=>Bool->Bool->Bool->Int->Projection_move->Maybe Int->Request a
simple_window_render_request strict_exist strict_match strict_capacity window_id projection_move maybe_sampler_id=Render {window_id=window_id,render_selector=Self_selector {value=()},projection_move=projection_move,maybe_sampler_id=maybe_sampler_id,strict_exist=strict_exist,strict_match=strict_match,strict_capacity=strict_capacity}

simple_canvas_render_request::ET.Has_call_stack=>Bool->Bool->Bool->Int->Projection_move->Maybe Int->Request a
simple_canvas_render_request strict_exist strict_match strict_capacity canvas_id projection_move maybe_sampler_id=Canvas_render {canvas_id=canvas_id,canvas_render_selector=Self_selector {value=()},projection_move=projection_move,maybe_sampler_id=maybe_sampler_id,strict_exist=strict_exist,strict_match=strict_match,strict_capacity=strict_capacity}

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
        Matrix {x_x=second_x_x,x_y=second_x_y,y_x=second_y_x,y_y=second_y_y}->Matrix {x=first_x,y=first_y,x_x=first_x_x*second_x_x+first_x_y*second_y_x,x_y=first_x_x*second_x_y+first_x_y*second_y_y,y_x=first_y_x*second_x_x+first_y_y*second_y_x,y_y=first_y_x*second_x_y+first_y_y*second_y_y}

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

quick_create_engine::ET.Has_call_stack=>Custom_state a->(Event a->Engine a->Maybe Int)->(Event a->Engine a->Projection_strategy)->FCT.CFloat->FCT.CFloat->FCT.CInt->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->Maybe DW.Word64->Int->Int->Sampler_create_info->Blend_state->Bool->Bool->IO (Engine a)
quick_create_engine state main_id projection_strategy font_size pixel_range max_picture_size max_vertex_size max_index_size max_parameter_size padding width height maybe_interval exponent_width exponent_height sampler_create_info blend_state strict_exist strict_match=case maybe_interval of
    Nothing->create_engine state main_id projection_strategy font_size pixel_range (max_picture_size*mebibyte) (max_vertex_size*mebibyte) (max_index_size*mebibyte) (max_parameter_size*mebibyte) padding width height 0 Nothing 0 0 0 0 exponent_width exponent_height sampler_create_info blend_state strict_exist strict_match
    Just interval->create_engine state main_id projection_strategy font_size pixel_range (max_picture_size*mebibyte) (max_vertex_size*mebibyte) (max_index_size*mebibyte) (max_parameter_size*mebibyte) padding width height 0 (Just (div nanosecond interval)) 0 0 0 0 exponent_width exponent_height sampler_create_info blend_state strict_exist strict_match

object_strategy_coincident::ET.Has_call_stack=>Projection_strategy
object_strategy_coincident=Object_strategy

image_strategy_strict::ET.Has_call_stack=>Projection_strategy
image_strategy_strict=Image_strategy {strict_exist=True}

image_strategy_lenient::ET.Has_call_stack=>Projection_strategy
image_strategy_lenient=Image_strategy {strict_exist=False}

object_path_coincident::ET.Has_call_stack=>Int->Projection_path
object_path_coincident leaf_id=Object_path {leaf_id=leaf_id}

image_path_strict::ET.Has_call_stack=>Int->Projection_path
image_path_strict leaf_id=Image_path {leaf_id=leaf_id,strict_exist=True}

image_path_lenient::ET.Has_call_stack=>Int->Projection_path
image_path_lenient leaf_id=Image_path {leaf_id=leaf_id,strict_exist=False}

object_move_coincident::ET.Has_call_stack=>Int->Bool->Projection_move
object_move_coincident leaf_id consume=Object_move {leaf_id=leaf_id,consume=consume}

image_move_strict::ET.Has_call_stack=>Int->Projection_move
image_move_strict leaf_id=Image_move {leaf_id=leaf_id,strict_exist=True}

image_move_lenient::ET.Has_call_stack=>Int->Projection_move
image_move_lenient leaf_id=Image_move {leaf_id=leaf_id,strict_exist=False}

none_selector_coincident::ET.Has_call_stack=>Selector a
none_selector_coincident=None_selector

combine_selector_coincident::ET.Has_call_stack=>DS.Seq (Selector a)->Selector a
combine_selector_coincident combine_selector=Combine_selector {combine_selector=combine_selector}

self_selector_coincident::ET.Has_call_stack=>a->Selector a
self_selector_coincident value=Self_selector {value=value}

all_selector_coincident::ET.Has_call_stack=>Maybe a->a->Selector a
all_selector_coincident maybe_value value=All_selector {maybe_value=maybe_value,value=value}

trigger_selector_strict::ET.Has_call_stack=>Maybe a->a->Selector a
trigger_selector_strict maybe_value value=Trigger_selector {maybe_value=maybe_value,value=value,strict_exist=True}

trigger_selector_lenient::ET.Has_call_stack=>Maybe a->a->Selector a
trigger_selector_lenient maybe_value value=Trigger_selector {maybe_value=maybe_value,value=value,strict_exist=False}

default_selector_strict::ET.Has_call_stack=>Maybe a->a->Selector a
default_selector_strict maybe_value value=Default_selector {maybe_value=maybe_value,value=value,strict_exist=True}

default_selector_lenient::ET.Has_call_stack=>Maybe a->a->Selector a
default_selector_lenient maybe_value value=Default_selector {maybe_value=maybe_value,value=value,strict_exist=False}

hosted_selector_strict::ET.Has_call_stack=>Maybe a->Selector a->Selector a
hosted_selector_strict maybe_value selector=Hosted_selector {maybe_value=maybe_value,selector=selector,strict_exist=True,strict_match=True}

hosted_selector_lenient::ET.Has_call_stack=>Maybe a->Selector a->Selector a
hosted_selector_lenient maybe_value selector=Hosted_selector {maybe_value=maybe_value,selector=selector,strict_exist=False,strict_match=False}

any_selector_strict::ET.Has_call_stack=>Maybe a->Selector a->Selector a
any_selector_strict maybe_value selector=Any_selector {maybe_value=maybe_value,selector=selector,strict_match=True}

any_selector_lenient::ET.Has_call_stack=>Maybe a->Selector a->Selector a
any_selector_lenient maybe_value selector=Any_selector {maybe_value=maybe_value,selector=selector,strict_match=False}

group_selector_strict::ET.Has_call_stack=>Maybe a->DIM.IntMap (Selector a)->Selector a
group_selector_strict maybe_value group_selector=Group_selector {maybe_value=maybe_value,group_selector=group_selector,strict_exist=True,strict_match=True}

group_selector_lenient::ET.Has_call_stack=>Maybe a->DIM.IntMap (Selector a)->Selector a
group_selector_lenient maybe_value group_selector=Group_selector {maybe_value=maybe_value,group_selector=group_selector,strict_exist=False,strict_match=False}

vector_selector_strict::ET.Has_call_stack=>Maybe a->DIM.IntMap (Selector a)->Selector a
vector_selector_strict maybe_value vector_selector=Vector_selector {maybe_value=maybe_value,vector_selector=vector_selector,strict_exist=True,strict_match=True}

vector_selector_lenient::ET.Has_call_stack=>Maybe a->DIM.IntMap (Selector a)->Selector a
vector_selector_lenient maybe_value vector_selector=Vector_selector {maybe_value=maybe_value,vector_selector=vector_selector,strict_exist=False,strict_match=False}

widget_trigger_selector_strict::ET.Has_call_stack=>Maybe a->Selector a->Selector a
widget_trigger_selector_strict maybe_value selector=Widget_trigger_selector {maybe_value=maybe_value,selector=selector,strict_match=True}

widget_trigger_selector_lenient::ET.Has_call_stack=>Maybe a->Selector a->Selector a
widget_trigger_selector_lenient maybe_value selector=Widget_trigger_selector {maybe_value=maybe_value,selector=selector,strict_match=False}

widget_io_trigger_selector_strict::ET.Has_call_stack=>Maybe a->Selector a->Selector a
widget_io_trigger_selector_strict maybe_value selector=Widget_io_trigger_selector {maybe_value=maybe_value,selector=selector,strict_match=True}

widget_io_trigger_selector_lenient::ET.Has_call_stack=>Maybe a->Selector a->Selector a
widget_io_trigger_selector_lenient maybe_value selector=Widget_io_trigger_selector {maybe_value=maybe_value,selector=selector,strict_match=False}

widget_mix_trigger_selector_strict::ET.Has_call_stack=>Maybe a->Selector a->Selector a
widget_mix_trigger_selector_strict maybe_value selector=Widget_mix_trigger_selector {maybe_value=maybe_value,selector=selector,strict_match=True}

widget_mix_trigger_selector_lenient::ET.Has_call_stack=>Maybe a->Selector a->Selector a
widget_mix_trigger_selector_lenient maybe_value selector=Widget_mix_trigger_selector {maybe_value=maybe_value,selector=selector,strict_match=False}

coroutine_selector_strict::ET.Has_call_stack=>Maybe a->DIM.IntMap (Selector a)->Selector a
coroutine_selector_strict maybe_value coroutine_selector=Coroutine_selector {maybe_value=maybe_value,coroutine_selector=coroutine_selector,strict_exist=True,strict_match=True}

coroutine_selector_lenient::ET.Has_call_stack=>Maybe a->DIM.IntMap (Selector a)->Selector a
coroutine_selector_lenient maybe_value coroutine_selector=Coroutine_selector {maybe_value=maybe_value,coroutine_selector=coroutine_selector,strict_exist=False,strict_match=False}

none_visual_selector_coincident::ET.Has_call_stack=>Visual_selector a
none_visual_selector_coincident=None_visual_selector

combine_visual_selector_coincident::ET.Has_call_stack=>DS.Seq (Visual_selector a)->Visual_selector a
combine_visual_selector_coincident combine_visual_selector=Combine_visual_selector {combine_visual_selector=combine_visual_selector}

any_visual_selector_strict::ET.Has_call_stack=>a->Visual_selector a
any_visual_selector_strict value=Any_visual_selector {value=value,strict_match=True}

any_visual_selector_lenient::ET.Has_call_stack=>a->Visual_selector a
any_visual_selector_lenient value=Any_visual_selector {value=value,strict_match=False}

visual_trigger_selector_strict::ET.Has_call_stack=>a->Visual_selector a
visual_trigger_selector_strict value=Visual_trigger_selector {value=value,strict_match=True}

visual_trigger_selector_lenient::ET.Has_call_stack=>a->Visual_selector a
visual_trigger_selector_lenient value=Visual_trigger_selector {value=value,strict_match=False}

visual_io_trigger_selector_strict::ET.Has_call_stack=>a->Visual_selector a
visual_io_trigger_selector_strict value=Visual_io_trigger_selector {value=value,strict_match=True}

visual_io_trigger_selector_lenient::ET.Has_call_stack=>a->Visual_selector a
visual_io_trigger_selector_lenient value=Visual_io_trigger_selector {value=value,strict_match=False}

visual_mix_trigger_selector_strict::ET.Has_call_stack=>a->Visual_selector a
visual_mix_trigger_selector_strict value=Visual_mix_trigger_selector {value=value,strict_match=True}

visual_mix_trigger_selector_lenient::ET.Has_call_stack=>a->Visual_selector a
visual_mix_trigger_selector_lenient value=Visual_mix_trigger_selector {value=value,strict_match=False}

group_visual_selector_strict::ET.Has_call_stack=>DIM.IntMap a->Visual_selector a
group_visual_selector_strict group_value=Group_visual_selector {group_value=group_value,strict_exist=True,strict_match=True}

group_visual_selector_lenient::ET.Has_call_stack=>DIM.IntMap a->Visual_selector a
group_visual_selector_lenient group_value=Group_visual_selector {group_value=group_value,strict_exist=False,strict_match=False}

vector_visual_selector_strict::ET.Has_call_stack=>DIM.IntMap a->Visual_selector a
vector_visual_selector_strict vector_value=Vector_visual_selector {vector_value=vector_value,strict_exist=True,strict_match=True}

vector_visual_selector_lenient::ET.Has_call_stack=>DIM.IntMap a->Visual_selector a
vector_visual_selector_lenient vector_value=Vector_visual_selector {vector_value=vector_value,strict_exist=False,strict_match=False}

reset_timer_coincident::ET.Has_call_stack=>DW.Word64->Request a
reset_timer_coincident interval=Reset_timer {interval=interval}

stop_timer_strict::ET.Has_call_stack=>Request a
stop_timer_strict=Stop_timer {strict_match=True}

stop_timer_lenient::ET.Has_call_stack=>Request a
stop_timer_lenient=Stop_timer {strict_match=False}

create_widget_coincident::ET.Has_call_stack=>Int->Maybe Int->Widget_request a->Request a
create_widget_coincident leaf_id maybe_father_id widget_request=Create_widget {leaf_id=leaf_id,maybe_father_id=maybe_father_id,widget_request=widget_request}

remove_widget_strict::ET.Has_call_stack=>Int->Request a
remove_widget_strict leaf_id=Remove_widget {leaf_id=leaf_id,strict_exist=True}

remove_widget_lenient::ET.Has_call_stack=>Int->Request a
remove_widget_lenient leaf_id=Remove_widget {leaf_id=leaf_id,strict_exist=False}

create_node_coincident::ET.Has_call_stack=>Int->Maybe Int->(Engine a->Event a->Event a)->(Event a->Engine a->Widget a->Widget a)->Request a
create_node_coincident node_id maybe_father_id event_transform widget_transform=Create_node {node_id=node_id,maybe_father_id=maybe_father_id,event_transform=event_transform,widget_transform=widget_transform}

remove_node_strict::ET.Has_call_stack=>Int->Request a
remove_node_strict node_id=Remove_node {node_id=node_id,strict_exist=True}

remove_node_lenient::ET.Has_call_stack=>Int->Request a
remove_node_lenient node_id=Remove_node {node_id=node_id,strict_exist=False}

create_window_coincident::ET.Has_call_stack=>Int->DT.Text->FCT.CInt->FCT.CInt->Color->DHS.HashSet Window_flag->Blend_state->Request a
create_window_coincident window_id title window_width window_height color window_flag blend_state=Create_window {window_id=window_id,title=title,window_width=window_width,window_height=window_height,color=color,window_flag=window_flag,blend_state=blend_state}

remove_window_strict::ET.Has_call_stack=>Int->Request a
remove_window_strict window_id=Remove_window {window_id=window_id,strict_exist=True}

remove_window_lenient::ET.Has_call_stack=>Int->Request a
remove_window_lenient window_id=Remove_window {window_id=window_id,strict_exist=False}

create_canvas_coincident::ET.Has_call_stack=>DW.Word32->DW.Word32->Maybe Int->Request a
create_canvas_coincident canvas_width canvas_height maybe_canvas_id=Create_canvas {canvas_width=canvas_width,canvas_height=canvas_height,maybe_canvas_id=maybe_canvas_id}

remove_canvas_strict::ET.Has_call_stack=>Int->Request a
remove_canvas_strict canvas_id=Remove_canvas {canvas_id=canvas_id,strict_exist=True,strict_match=True}

remove_canvas_lenient::ET.Has_call_stack=>Int->Request a
remove_canvas_lenient canvas_id=Remove_canvas {canvas_id=canvas_id,strict_exist=False,strict_match=False}

create_shader_coincident::ET.Has_call_stack=>Int->DW.Word32->DW.Word32->DW.Word32->DT.Text->Request a
create_shader_coincident shader_id stage num_sampler num_uniform_buffer path=Create_shader {shader_id=shader_id,stage=stage,num_sampler=num_sampler,num_uniform_buffer=num_uniform_buffer,path=path}

remove_shader_strict::ET.Has_call_stack=>Int->Request a
remove_shader_strict shader_id=Remove_shader {shader_id=shader_id,strict_exist=True,strict_resource=True}

remove_shader_lenient::ET.Has_call_stack=>Int->Request a
remove_shader_lenient shader_id=Remove_shader {shader_id=shader_id,strict_exist=False,strict_resource=False}

create_pipeline_strict::ET.Has_call_stack=>Maybe Int->Int->Int->Blend_state->Request a
create_pipeline_strict maybe_vertex_shader_id fragment_shader_id pipeline_id blend_state=Create_pipeline {maybe_vertex_shader_id=maybe_vertex_shader_id,fragment_shader_id=fragment_shader_id,pipeline_id=pipeline_id,blend_state=blend_state,strict_exist=True}

create_pipeline_lenient::ET.Has_call_stack=>Maybe Int->Int->Int->Blend_state->Request a
create_pipeline_lenient maybe_vertex_shader_id fragment_shader_id pipeline_id blend_state=Create_pipeline {maybe_vertex_shader_id=maybe_vertex_shader_id,fragment_shader_id=fragment_shader_id,pipeline_id=pipeline_id,blend_state=blend_state,strict_exist=False}

remove_pipeline_strict::ET.Has_call_stack=>Int->Request a
remove_pipeline_strict pipeline_id=Remove_pipeline {pipeline_id=pipeline_id,strict_exist=True}

remove_pipeline_lenient::ET.Has_call_stack=>Int->Request a
remove_pipeline_lenient pipeline_id=Remove_pipeline {pipeline_id=pipeline_id,strict_exist=False}

create_sampler_coincident::ET.Has_call_stack=>Int->Sampler_create_info->Request a
create_sampler_coincident sampler_id sampler_create_info=Create_sampler {sampler_id=sampler_id,sampler_create_info=sampler_create_info}

remove_sampler_strict::ET.Has_call_stack=>Int->Request a
remove_sampler_strict sampler_id=Remove_sampler {sampler_id=sampler_id,strict_exist=True}

remove_sampler_lenient::ET.Has_call_stack=>Int->Request a
remove_sampler_lenient sampler_id=Remove_sampler {sampler_id=sampler_id,strict_exist=False}

create_atlas_font_strict::ET.Has_call_stack=>Int->Int->Int->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->DT.Text->Maybe (DHS.HashSet Char)->Request a
create_atlas_font_strict atlas_font_id exponent_width exponent_height padding width height font_size pixel_range path maybe_charset=Create_atlas_font {atlas_font_id=atlas_font_id,exponent_width=exponent_width,exponent_height=exponent_height,padding=padding,width=width,height=height,font_size=font_size,pixel_range=pixel_range,path=path,maybe_charset=maybe_charset,strict_exist=True}

create_atlas_font_lenient::ET.Has_call_stack=>Int->Int->Int->DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->DT.Text->Maybe (DHS.HashSet Char)->Request a
create_atlas_font_lenient atlas_font_id exponent_width exponent_height padding width height font_size pixel_range path maybe_charset=Create_atlas_font {atlas_font_id=atlas_font_id,exponent_width=exponent_width,exponent_height=exponent_height,padding=padding,width=width,height=height,font_size=font_size,pixel_range=pixel_range,path=path,maybe_charset=maybe_charset,strict_exist=False}

remove_atlas_font_strict::ET.Has_call_stack=>Int->Request a
remove_atlas_font_strict atlas_font_id=Remove_atlas_font {atlas_font_id=atlas_font_id,strict_exist=True,strict_resource=True}

remove_atlas_font_lenient::ET.Has_call_stack=>Int->Request a
remove_atlas_font_lenient atlas_font_id=Remove_atlas_font {atlas_font_id=atlas_font_id,strict_exist=False,strict_resource=False}

set_window_icon_strict::ET.Has_call_stack=>Int->DT.Text->Request a
set_window_icon_strict window_id path=Set_window_icon {window_id=window_id,path=path,strict_exist=True}

set_window_icon_lenient::ET.Has_call_stack=>Int->DT.Text->Request a
set_window_icon_lenient window_id path=Set_window_icon {window_id=window_id,path=path,strict_exist=False}

set_window_size_strict::ET.Has_call_stack=>Int->FCT.CInt->FCT.CInt->Request a
set_window_size_strict window_id window_width window_height=Set_window_size {window_id=window_id,window_width=window_width,window_height=window_height,strict_exist=True}

set_window_size_lenient::ET.Has_call_stack=>Int->FCT.CInt->FCT.CInt->Request a
set_window_size_lenient window_id window_width window_height=Set_window_size {window_id=window_id,window_width=window_width,window_height=window_height,strict_exist=False}

set_window_position_strict::ET.Has_call_stack=>Int->FCT.CInt->FCT.CInt->Request a
set_window_position_strict window_id x y=Set_window_position {window_id=window_id,x=x,y=y,strict_exist=True}

set_window_position_lenient::ET.Has_call_stack=>Int->FCT.CInt->FCT.CInt->Request a
set_window_position_lenient window_id x y=Set_window_position {window_id=window_id,x=x,y=y,strict_exist=False}

set_window_title_strict::ET.Has_call_stack=>Int->DT.Text->Request a
set_window_title_strict window_id title=Set_window_title {window_id=window_id,title=title,strict_exist=True}

set_window_title_lenient::ET.Has_call_stack=>Int->DT.Text->Request a
set_window_title_lenient window_id title=Set_window_title {window_id=window_id,title=title,strict_exist=False}

set_window_fullscreen_strict::ET.Has_call_stack=>Int->Bool->Request a
set_window_fullscreen_strict window_id fullscreen=Set_window_fullscreen {window_id=window_id,fullscreen=fullscreen,strict_exist=True}

set_window_fullscreen_lenient::ET.Has_call_stack=>Int->Bool->Request a
set_window_fullscreen_lenient window_id fullscreen=Set_window_fullscreen {window_id=window_id,fullscreen=fullscreen,strict_exist=False}

set_system_cursor_strict::ET.Has_call_stack=>System_cursor->Request a
set_system_cursor_strict system_cursor=Set_system_cursor {system_cursor=system_cursor,strict_exist=True}

set_system_cursor_lenient::ET.Has_call_stack=>System_cursor->Request a
set_system_cursor_lenient system_cursor=Set_system_cursor {system_cursor=system_cursor,strict_exist=False}

clean_atlas_strict::ET.Has_call_stack=>Request a
clean_atlas_strict=Clean_atlas {strict_exist=True}

clean_atlas_lenient::ET.Has_call_stack=>Request a
clean_atlas_lenient=Clean_atlas {strict_exist=False}

unlock_strict::ET.Has_call_stack=>Int->Request a
unlock_strict leaf_id=Unlock {leaf_id=leaf_id,strict_exist=True}

unlock_lenient::ET.Has_call_stack=>Int->Request a
unlock_lenient leaf_id=Unlock {leaf_id=leaf_id,strict_exist=False}

update_font_coincident::ET.Has_call_stack=>DT.Text->Maybe (DHS.HashSet Char)->Request a
update_font_coincident path maybe_charset=Update_font {path=path,maybe_charset=maybe_charset}

update_atlas_font_strict::ET.Has_call_stack=>Int->DT.Text->Maybe (DHS.HashSet Char)->Request a
update_atlas_font_strict atlas_font_id path maybe_charset=Update_atlas_font {atlas_font_id=atlas_font_id,path=path,maybe_charset=maybe_charset,strict_exist=True}

update_atlas_font_lenient::ET.Has_call_stack=>Int->DT.Text->Maybe (DHS.HashSet Char)->Request a
update_atlas_font_lenient atlas_font_id path maybe_charset=Update_atlas_font {atlas_font_id=atlas_font_id,path=path,maybe_charset=maybe_charset,strict_exist=False}

render_strict::ET.Has_call_stack=>Int->Selector ()->Projection_move->Maybe Int->Request a
render_strict window_id render_selector projection_move maybe_sampler_id=Render {window_id=window_id,render_selector=render_selector,projection_move=projection_move,maybe_sampler_id=maybe_sampler_id,strict_exist=True,strict_match=True,strict_capacity=True}

render_lenient::ET.Has_call_stack=>Int->Selector ()->Projection_move->Maybe Int->Request a
render_lenient window_id render_selector projection_move maybe_sampler_id=Render {window_id=window_id,render_selector=render_selector,projection_move=projection_move,maybe_sampler_id=maybe_sampler_id,strict_exist=False,strict_match=False,strict_capacity=False}

canvas_render_strict::ET.Has_call_stack=>Int->Selector ()->Projection_move->Maybe Int->Request a
canvas_render_strict canvas_id canvas_render_selector projection_move maybe_sampler_id=Canvas_render {canvas_id=canvas_id,canvas_render_selector=canvas_render_selector,projection_move=projection_move,maybe_sampler_id=maybe_sampler_id,strict_exist=True,strict_match=True,strict_capacity=True}

canvas_render_lenient::ET.Has_call_stack=>Int->Selector ()->Projection_move->Maybe Int->Request a
canvas_render_lenient canvas_id canvas_render_selector projection_move maybe_sampler_id=Canvas_render {canvas_id=canvas_id,canvas_render_selector=canvas_render_selector,projection_move=projection_move,maybe_sampler_id=maybe_sampler_id,strict_exist=False,strict_match=False,strict_capacity=False}

canvas_widget_render_strict::ET.Has_call_stack=>Projection_path->Selector (Selector ())->Projection_move->Maybe Int->Request a
canvas_widget_render_strict projection_path canvas_widget_render_selector projection_move maybe_sampler_id=Canvas_widget_render {projection_path=projection_path,canvas_widget_render_selector=canvas_widget_render_selector,projection_move=projection_move,maybe_sampler_id=maybe_sampler_id,strict_exist=True,strict_match=True,strict_capacity=True}

canvas_widget_render_lenient::ET.Has_call_stack=>Projection_path->Selector (Selector ())->Projection_move->Maybe Int->Request a
canvas_widget_render_lenient projection_path canvas_widget_render_selector projection_move maybe_sampler_id=Canvas_widget_render {projection_path=projection_path,canvas_widget_render_selector=canvas_widget_render_selector,projection_move=projection_move,maybe_sampler_id=maybe_sampler_id,strict_exist=False,strict_match=False,strict_capacity=False}

shader_canvas_strict::ET.Has_call_stack=>Uniform->Int->Int->Maybe Int->Request a
shader_canvas_strict uniform canvas_id pipeline_id maybe_sampler_id=Shader_canvas {uniform=uniform,canvas_id=canvas_id,pipeline_id=pipeline_id,maybe_sampler_id=maybe_sampler_id,strict_exist=True}

shader_canvas_lenient::ET.Has_call_stack=>Uniform->Int->Int->Maybe Int->Request a
shader_canvas_lenient uniform canvas_id pipeline_id maybe_sampler_id=Shader_canvas {uniform=uniform,canvas_id=canvas_id,pipeline_id=pipeline_id,maybe_sampler_id=maybe_sampler_id,strict_exist=False}

io_coincident::ET.Has_call_stack=>(Engine a->IO (Engine a))->Request a
io_coincident io=Io {io=io}

step_animation_strict::ET.Has_call_stack=>FCT.CFloat->Int->Selector (Visual_selector Bool)->Engine a->Engine a
step_animation_strict=step_animation True True

step_animation_lenient::ET.Has_call_stack=>FCT.CFloat->Int->Selector (Visual_selector Bool)->Engine a->Engine a
step_animation_lenient=step_animation False False

step_animation_visual_strict::ET.Has_call_stack=>Bool->FCT.CFloat->Visual a->Visual a
step_animation_visual_strict=step_animation_visual True

step_animation_visual_lenient::ET.Has_call_stack=>Bool->FCT.CFloat->Visual a->Visual a
step_animation_visual_lenient=step_animation_visual False

clean_collect_strict::ET.Has_call_stack=>Int->Selector a->Engine b->Engine b
clean_collect_strict=clean_collect True

clean_collect_lenient::ET.Has_call_stack=>Int->Selector a->Engine b->Engine b
clean_collect_lenient=clean_collect False

collect_canvas_strict::ET.Has_call_stack=>Arrange->Maybe (Border FCT.CFloat)->Int->Int->Selector a->Insert_strategy->Engine b->Engine b
collect_canvas_strict=collect_canvas True True

collect_canvas_lenient::ET.Has_call_stack=>Arrange->Maybe (Border FCT.CFloat)->Int->Int->Selector a->Insert_strategy->Engine b->Engine b
collect_canvas_lenient=collect_canvas False False

maybe_update_collect_strict::ET.Has_call_stack=>Custom a=>(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector b->Visual_selector c->Insert_strategy->Engine a->Engine a
maybe_update_collect_strict=maybe_update_collect True True True

maybe_update_collect_lenient::ET.Has_call_stack=>Custom a=>(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector b->Visual_selector c->Insert_strategy->Engine a->Engine a
maybe_update_collect_lenient=maybe_update_collect False False False

maybe_collect_update_strict::ET.Has_call_stack=>Custom a=>(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector b->Visual_selector c->Insert_strategy->Engine a->Engine a
maybe_collect_update_strict=maybe_collect_update True True True

maybe_collect_update_lenient::ET.Has_call_stack=>Custom a=>(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector b->Visual_selector c->Insert_strategy->Engine a->Engine a
maybe_collect_update_lenient=maybe_collect_update False False False

collect_strict::ET.Has_call_stack=>Custom a=>(Widget a->Widget a)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector b->Visual_selector c->Insert_strategy->Engine a->Engine a
collect_strict=collect True True True

collect_lenient::ET.Has_call_stack=>Custom a=>(Widget a->Widget a)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector b->Visual_selector c->Insert_strategy->Engine a->Engine a
collect_lenient=collect False False False

move_strict::ET.Has_call_stack=>Projection_move->Int->Selector (Selector Insert_strategy)->Engine a->Engine a
move_strict=move True True

move_lenient::ET.Has_call_stack=>Projection_move->Int->Selector (Selector Insert_strategy)->Engine a->Engine a
move_lenient=move False False

run_coroutine_strict::ET.Has_call_stack=>Int->Selector a->DS.Seq Int->Event b->Engine b->Engine b
run_coroutine_strict=run_coroutine True True

run_coroutine_lenient::ET.Has_call_stack=>Int->Selector a->DS.Seq Int->Event b->Engine b->Engine b
run_coroutine_lenient=run_coroutine False False

create_image_strict::ET.Has_call_stack=>Int->Event a->Engine a->Engine a
create_image_strict leaf_id event engine=create_image True True leaf_id event engine

create_image_lenient::ET.Has_call_stack=>Int->Event a->Engine a->Engine a
create_image_lenient leaf_id event engine=create_image False False leaf_id event engine

remove_image_strict::ET.Has_call_stack=>Int->Engine a->Engine a
remove_image_strict leaf_id engine=remove_image True leaf_id engine

remove_image_lenient::ET.Has_call_stack=>Int->Engine a->Engine a
remove_image_lenient leaf_id engine=remove_image False leaf_id engine

scroll_text_strict::ET.Has_call_stack=>FCT.CFloat->Visual a->Visual a
scroll_text_strict=scroll_text True

scroll_text_lenient::ET.Has_call_stack=>FCT.CFloat->Visual a->Visual a
scroll_text_lenient=scroll_text False

scroll_top_text_strict::ET.Has_call_stack=>Visual a->Visual a
scroll_top_text_strict=scroll_top_text True

scroll_top_text_lenient::ET.Has_call_stack=>Visual a->Visual a
scroll_top_text_lenient=scroll_top_text False

scroll_bottom_text_strict::ET.Has_call_stack=>Visual a->Visual a
scroll_bottom_text_strict=scroll_bottom_text True

scroll_bottom_text_lenient::ET.Has_call_stack=>Visual a->Visual a
scroll_bottom_text_lenient=scroll_bottom_text False

update_store_widget_strict::ET.Has_call_stack=>Convert Data a=>Convert a Data=>(a->a)->Widget b->Widget b
update_store_widget_strict=update_store_widget True

update_store_widget_lenient::ET.Has_call_stack=>Convert Data a=>Convert a Data=>(a->a)->Widget b->Widget b
update_store_widget_lenient=update_store_widget False

update_group_visual_strict::ET.Has_call_stack=>Int->(Visual a->Visual a)->Widget a->Widget a
update_group_visual_strict=update_group_visual True True

update_group_visual_lenient::ET.Has_call_stack=>Int->(Visual a->Visual a)->Widget a->Widget a
update_group_visual_lenient=update_group_visual False False

update_vector_visual_strict::ET.Has_call_stack=>Int->(Visual a->Visual a)->Widget a->Widget a
update_vector_visual_strict=update_vector_visual True True

update_vector_visual_lenient::ET.Has_call_stack=>Int->(Visual a->Visual a)->Widget a->Widget a
update_vector_visual_lenient=update_vector_visual False False

update_vector_widget_strict::ET.Has_call_stack=>Int->(Widget a->Widget a)->Widget a->Widget a
update_vector_widget_strict=update_vector_widget True True

update_vector_widget_lenient::ET.Has_call_stack=>Int->(Widget a->Widget a)->Widget a->Widget a
update_vector_widget_lenient=update_vector_widget False False

hosted_update_vector_widget_strict::ET.Has_call_stack=>(Widget a->Widget a)->Widget a->Widget a
hosted_update_vector_widget_strict=hosted_update_vector_widget True True

hosted_update_vector_widget_lenient::ET.Has_call_stack=>(Widget a->Widget a)->Widget a->Widget a
hosted_update_vector_widget_lenient=hosted_update_vector_widget False False

update_group_widget_strict::ET.Has_call_stack=>Int->(Widget a->Widget a)->Widget a->Widget a
update_group_widget_strict=update_group_widget True True

update_group_widget_lenient::ET.Has_call_stack=>Int->(Widget a->Widget a)->Widget a->Widget a
update_group_widget_lenient=update_group_widget False False

hosted_update_group_widget_strict::ET.Has_call_stack=>(Widget a->Widget a)->Widget a->Widget a
hosted_update_group_widget_strict=hosted_update_group_widget True True

hosted_update_group_widget_lenient::ET.Has_call_stack=>(Widget a->Widget a)->Widget a->Widget a
hosted_update_group_widget_lenient=hosted_update_group_widget False False

from_same_insert_widget_strict::ET.Has_call_stack=>Int->DS.Seq Insert_strategy->Widget a->Engine a->Engine a
from_same_insert_widget_strict=from_same_insert_widget True True

from_same_insert_widget_lenient::ET.Has_call_stack=>Int->DS.Seq Insert_strategy->Widget a->Engine a->Engine a
from_same_insert_widget_lenient=from_same_insert_widget False False

from_insert_widget_strict::ET.Has_call_stack=>Int->DS.Seq (Insert (Widget a))->Engine a->Engine a
from_insert_widget_strict=from_insert_widget True True

from_insert_widget_lenient::ET.Has_call_stack=>Int->DS.Seq (Insert (Widget a))->Engine a->Engine a
from_insert_widget_lenient=from_insert_widget False False

create_adaptive_window_trigger_request_strict::ET.Has_call_stack=>(Event a->Engine a->Maybe Int)->DIS.IntSet->Widget_request a
create_adaptive_window_trigger_request_strict=create_adaptive_window_trigger_request True

create_adaptive_window_trigger_request_lenient::ET.Has_call_stack=>(Event a->Engine a->Maybe Int)->DIS.IntSet->Widget_request a
create_adaptive_window_trigger_request_lenient=create_adaptive_window_trigger_request False






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
{-# INLINE object_strategy_coincident #-}
{-# INLINE image_strategy_strict #-}
{-# INLINE image_strategy_lenient #-}
{-# INLINE object_path_coincident #-}
{-# INLINE image_path_strict #-}
{-# INLINE image_path_lenient #-}
{-# INLINE object_move_coincident #-}
{-# INLINE image_move_strict #-}
{-# INLINE image_move_lenient #-}
{-# INLINE none_selector_coincident #-}
{-# INLINE combine_selector_coincident #-}
{-# INLINE self_selector_coincident #-}
{-# INLINE all_selector_coincident #-}
{-# INLINE trigger_selector_strict #-}
{-# INLINE trigger_selector_lenient #-}
{-# INLINE default_selector_strict #-}
{-# INLINE default_selector_lenient #-}
{-# INLINE hosted_selector_strict #-}
{-# INLINE hosted_selector_lenient #-}
{-# INLINE any_selector_strict #-}
{-# INLINE any_selector_lenient #-}
{-# INLINE group_selector_strict #-}
{-# INLINE group_selector_lenient #-}
{-# INLINE vector_selector_strict #-}
{-# INLINE vector_selector_lenient #-}
{-# INLINE widget_trigger_selector_strict #-}
{-# INLINE widget_trigger_selector_lenient #-}
{-# INLINE widget_io_trigger_selector_strict #-}
{-# INLINE widget_io_trigger_selector_lenient #-}
{-# INLINE widget_mix_trigger_selector_strict #-}
{-# INLINE widget_mix_trigger_selector_lenient #-}
{-# INLINE coroutine_selector_strict #-}
{-# INLINE coroutine_selector_lenient #-}
{-# INLINE none_visual_selector_coincident #-}
{-# INLINE combine_visual_selector_coincident #-}
{-# INLINE any_visual_selector_strict #-}
{-# INLINE any_visual_selector_lenient #-}
{-# INLINE visual_trigger_selector_strict #-}
{-# INLINE visual_trigger_selector_lenient #-}
{-# INLINE visual_io_trigger_selector_strict #-}
{-# INLINE visual_io_trigger_selector_lenient #-}
{-# INLINE visual_mix_trigger_selector_strict #-}
{-# INLINE visual_mix_trigger_selector_lenient #-}
{-# INLINE group_visual_selector_strict #-}
{-# INLINE group_visual_selector_lenient #-}
{-# INLINE vector_visual_selector_strict #-}
{-# INLINE vector_visual_selector_lenient #-}
{-# INLINE reset_timer_coincident #-}
{-# INLINE stop_timer_strict #-}
{-# INLINE stop_timer_lenient #-}
{-# INLINE create_widget_coincident #-}
{-# INLINE remove_widget_strict #-}
{-# INLINE remove_widget_lenient #-}
{-# INLINE create_node_coincident #-}
{-# INLINE remove_node_strict #-}
{-# INLINE remove_node_lenient #-}
{-# INLINE create_window_coincident #-}
{-# INLINE remove_window_strict #-}
{-# INLINE remove_window_lenient #-}
{-# INLINE create_canvas_coincident #-}
{-# INLINE remove_canvas_strict #-}
{-# INLINE remove_canvas_lenient #-}
{-# INLINE create_shader_coincident #-}
{-# INLINE remove_shader_strict #-}
{-# INLINE remove_shader_lenient #-}
{-# INLINE create_pipeline_strict #-}
{-# INLINE create_pipeline_lenient #-}
{-# INLINE remove_pipeline_strict #-}
{-# INLINE remove_pipeline_lenient #-}
{-# INLINE create_sampler_coincident #-}
{-# INLINE remove_sampler_strict #-}
{-# INLINE remove_sampler_lenient #-}
{-# INLINE create_atlas_font_strict #-}
{-# INLINE create_atlas_font_lenient #-}
{-# INLINE remove_atlas_font_strict #-}
{-# INLINE remove_atlas_font_lenient #-}
{-# INLINE set_window_icon_strict #-}
{-# INLINE set_window_icon_lenient #-}
{-# INLINE set_window_size_strict #-}
{-# INLINE set_window_size_lenient #-}
{-# INLINE set_window_position_strict #-}
{-# INLINE set_window_position_lenient #-}
{-# INLINE set_window_title_strict #-}
{-# INLINE set_window_title_lenient #-}
{-# INLINE set_window_fullscreen_strict #-}
{-# INLINE set_window_fullscreen_lenient #-}
{-# INLINE set_system_cursor_strict #-}
{-# INLINE set_system_cursor_lenient #-}
{-# INLINE clean_atlas_strict #-}
{-# INLINE clean_atlas_lenient #-}
{-# INLINE unlock_strict #-}
{-# INLINE unlock_lenient #-}
{-# INLINE update_font_coincident #-}
{-# INLINE update_atlas_font_strict #-}
{-# INLINE update_atlas_font_lenient #-}
{-# INLINE render_strict #-}
{-# INLINE render_lenient #-}
{-# INLINE canvas_render_strict #-}
{-# INLINE canvas_render_lenient #-}
{-# INLINE canvas_widget_render_strict #-}
{-# INLINE canvas_widget_render_lenient #-}
{-# INLINE shader_canvas_strict #-}
{-# INLINE shader_canvas_lenient #-}
{-# INLINE io_coincident #-}
{-# INLINE step_animation_strict #-}
{-# INLINE step_animation_lenient #-}
{-# INLINE step_animation_visual_strict #-}
{-# INLINE step_animation_visual_lenient #-}
{-# INLINE clean_collect_strict #-}
{-# INLINE clean_collect_lenient #-}
{-# INLINE collect_canvas_strict #-}
{-# INLINE collect_canvas_lenient #-}
{-# INLINE maybe_update_collect_strict #-}
{-# INLINE maybe_update_collect_lenient #-}
{-# INLINE maybe_collect_update_strict #-}
{-# INLINE maybe_collect_update_lenient #-}
{-# INLINE collect_strict #-}
{-# INLINE collect_lenient #-}
{-# INLINE move_strict #-}
{-# INLINE move_lenient #-}
{-# INLINE run_coroutine_strict #-}
{-# INLINE run_coroutine_lenient #-}
{-# INLINE create_image_strict #-}
{-# INLINE create_image_lenient #-}
{-# INLINE remove_image_strict #-}
{-# INLINE remove_image_lenient #-}
{-# INLINE scroll_text_strict #-}
{-# INLINE scroll_text_lenient #-}
{-# INLINE scroll_top_text_strict #-}
{-# INLINE scroll_top_text_lenient #-}
{-# INLINE scroll_bottom_text_strict #-}
{-# INLINE scroll_bottom_text_lenient #-}
{-# INLINE update_store_widget_strict #-}
{-# INLINE update_store_widget_lenient #-}
{-# INLINE update_group_visual_strict #-}
{-# INLINE update_group_visual_lenient #-}
{-# INLINE update_vector_visual_strict #-}
{-# INLINE update_vector_visual_lenient #-}
{-# INLINE update_vector_widget_strict #-}
{-# INLINE update_vector_widget_lenient #-}
{-# INLINE hosted_update_vector_widget_strict #-}
{-# INLINE hosted_update_vector_widget_lenient #-}
{-# INLINE update_group_widget_strict #-}
{-# INLINE update_group_widget_lenient #-}
{-# INLINE hosted_update_group_widget_strict #-}
{-# INLINE hosted_update_group_widget_lenient #-}
{-# INLINE from_same_insert_widget_strict #-}
{-# INLINE from_same_insert_widget_lenient #-}
{-# INLINE from_insert_widget_strict #-}
{-# INLINE from_insert_widget_lenient #-}
{-# INLINE create_adaptive_window_trigger_request_strict #-}
{-# INLINE create_adaptive_window_trigger_request_lenient #-}