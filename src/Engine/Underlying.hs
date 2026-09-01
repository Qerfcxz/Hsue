{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Underlying where

import Engine.Container
import Engine.Type
import qualified SDL.Function as SDLF
import qualified SDL.Type as SDLT
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Control.Monad as CM
import qualified Control.Monad.ST as CMST
import qualified Control.Monad.Trans.State as CMTS
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

get_store_widget::ET.Has_call_stack=>Convert Data b=>Widget a->b
get_store_widget widget=case widget of
    Store {store}->convert store
    _->EF.empty_error

update_store_widget::ET.Has_call_stack=>Convert Data a=>Convert a Data=>(a->a)->Widget b->Widget b
update_store_widget update widget=case widget of
    Store {store}->Store {store=convert (update (convert store))}
    _->EF.empty_error

update_group_visual::ET.Has_call_stack=>Int->(Visual a->Visual a)->Widget a->Widget a
update_group_visual index update widget=case widget of
    Group_visual {arrange,group_visual}->Group_visual {arrange=arrange,group_visual=int_map_update index update group_visual}
    _->EF.empty_error

update_vector_visual::ET.Has_call_stack=>Int->(Visual a->Visual a)->Widget a->Widget a
update_vector_visual index update widget=case widget of
    Vector_visual {arrange,vector_visual}->Vector_visual {arrange=arrange,vector_visual=CMST.runST (action_vector (\this_vector_visual->DVM.write this_vector_visual index (update (vector_visual DV.! index))) vector_visual)}
    _->EF.empty_error

update_vector_widget::ET.Has_call_stack=>Int->(Widget a->Widget a)->Widget a->Widget a
update_vector_widget this_index update widget=case widget of
    Vector {index,vector_widget}->Vector {index=index,vector_widget=CMST.runST (action_vector (\this_vector_widget->DVM.write this_vector_widget this_index (update (vector_widget DV.! this_index))) vector_widget)}
    _->EF.empty_error

hosted_update_vector_widget::ET.Has_call_stack=>(Widget a->Widget a)->Widget a->Widget a
hosted_update_vector_widget update widget=case widget of
    Vector {index,vector_widget}->Vector {index=index,vector_widget=CMST.runST (action_vector (\this_vector_widget->DVM.write this_vector_widget index (update (vector_widget DV.! index))) vector_widget)}
    _->EF.empty_error

action_vector::ET.Has_call_stack=>DVM.PrimMonad a=>(DVM.MVector (DVM.PrimState a) b->a ())->DV.Vector b->a (DV.Vector b)
action_vector action vector_widget=do
    new_vector_widget<-DV.thaw vector_widget
    action new_vector_widget
    DV.unsafeFreeze new_vector_widget

update_group_widget::ET.Has_call_stack=>Int->(Widget a->Widget a)->Widget a->Widget a
update_group_widget this_index update widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=int_map_update this_index update group_widget}
    _->EF.empty_error

hosted_update_group_widget::ET.Has_call_stack=>(Widget a->Widget a)->Widget a->Widget a
hosted_update_group_widget update widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=int_map_update index update group_widget}
    _->EF.empty_error

update_coroutine_state::ET.Has_call_stack=>(Widget a->Widget a)->Coroutine_state a->Coroutine_state a
update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->Coroutine_state {widget=update widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}

functor_update_coroutine_state::ET.Has_call_stack=>Functor b=>(Widget a->b (Widget a))->Coroutine_state a->b (Coroutine_state a)
functor_update_coroutine_state update coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->fmap (\this_widget->Coroutine_state {widget=this_widget,variable=variable,user_variable=user_variable,program_counter=program_counter,index_group=index_group,main_index_group=main_index_group,index_group_index=index_group_index,program_counter_index=program_counter_index}) (update widget)

get_sdl_pipeline::ET.Has_call_stack=>Pipeline->FP.Ptr SDLT.SDL_GPUGraphicsPipeline
get_sdl_pipeline pipeline=case pipeline of
    Pipeline {sdl_pipeline}->sdl_pipeline
    Default_pipeline {sdl_pipeline}->sdl_pipeline

update_shader_reference::ET.Has_call_stack=>(Int->Int)->Shader->Shader
update_shader_reference update shader=case shader of
    Shader {sdl_shader,reference}->Shader {sdl_shader=sdl_shader,reference=update reference}

widget_lookup::ET.Has_call_stack=>Widget a->Widget a
widget_lookup this_widget=case this_widget of
    Group {index,group_widget}->widget_lookup (int_map_lookup index group_widget)
    Vector {index,vector_widget}->widget_lookup (vector_widget DV.! index)
    Widget_trigger {widget}->widget_lookup widget
    Widget_io_trigger {widget}->widget_lookup widget
    Widget_mix_trigger {widget}->widget_lookup widget
    Coroutine {index,coroutine_state}->widget_lookup (int_map_lookup index coroutine_state).widget
    _->this_widget

lock_visual::ET.Has_call_stack=>Custom a=>Visual a->Visual a
lock_visual visual=case visual of
    Picture {arrange,half_width,half_height,min_u,min_v,max_u,max_v,path}->Picture {arrange=arrange,half_width=half_width,half_height=half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,path=path,locked=True}
    Atlas {arrange,path,clip_request,clip,index}->Atlas {arrange=arrange,path=path,clip_request=clip_request,clip=clip,index=index,locked=True}
    Text {arrange,half_width,half_height,current_y,min_y,max_y,anchor,article,charset}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=current_y,min_y=min_y,max_y=max_y,anchor=anchor,article=article,charset=charset,locked=True}
    Custom_visual {custom}->Custom_visual {custom=custom_visual_lock custom}
    _->visual

sdl_error::ET.Has_call_stack=>IO a
sdl_error=do
    ptr<-SDLF.sdl_get_error
    string<-FCS.peekCString ptr
    error string

sdl_catch_false::ET.Has_call_stack=>IO FCT.CBool->IO ()
sdl_catch_false io=do
    value<-io
    CM.unless (FMU.toBool value) sdl_error

sdl_catch_zero::ET.Has_call_stack=>Eq a=>Num a=>a->IO ()
sdl_catch_zero number=case number of
    0->sdl_error
    _->return ()

sdl_catch_null::ET.Has_call_stack=>FP.Ptr a->IO ()
sdl_catch_null ptr=CM.when (ptr==FP.nullPtr) sdl_error

sdl_return_catch_null::ET.Has_call_stack=>IO (FP.Ptr a)->IO (FP.Ptr a)
sdl_return_catch_null io=do
    ptr<-io
    if ptr==FP.nullPtr then sdl_error else return ptr

catch_null::ET.Has_call_stack=>FP.Ptr a->IO ()
catch_null ptr=CM.when (ptr==FP.nullPtr) EF.empty_error

with_string::ET.Has_call_stack=>String->(FP.Ptr FCT.CChar->IO a)->IO a
with_string string=DBS.useAsCString (DTE.encodeUtf8 (DT.pack string))

seq_poke_array::ET.Has_call_stack=>FS.Storable a=>Int->DS.Seq a->FP.Ptr a->IO ()
seq_poke_array size value ptr=CM.void (DF.foldlM (flip (seq_poke_array_a size)) ptr value)

seq_poke_array_a::ET.Has_call_stack=>FS.Storable a=>Int->a->FP.Ptr a->IO (FP.Ptr a)
seq_poke_array_a size value ptr=do
    FS.poke ptr value
    return (FP.plusPtr ptr size)

triple_reverse::ET.Has_call_stack=>(a,b,c)->(c,b,a)
triple_reverse (a,b,c)=(c,b,a)

monad_action_swap::ET.Has_call_stack=>Monad c=>(a->b->c (d,e))->a->b->c (e,d)
monad_action_swap action first_value second_value=do
    (new_first_value,new_second_value)<-action first_value second_value
    return (new_second_value,new_first_value)

vector_io_map::ET.Has_call_stack=>(a->b->IO (b,c))->DV.Vector a->b->IO (b,DV.Vector c)
vector_io_map action vector value=do
    (new_vector,new_value)<-CMTS.runStateT (DV.mapM (\this_value->CMTS.StateT {runStateT=monad_action_swap action this_value}) vector) value
    return (new_value,new_vector)

move_clip::ET.Has_call_stack=>Point->Clip->Clip
move_clip point clip=case clip of
    Clip {x,y,half_width,half_height,min_u,min_v,max_u,max_v}->Clip {x=x+point.x,y=y+point.y,half_width=half_width,half_height=half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v}

multiply_color::ET.Has_call_stack=>Color->Color->Color
multiply_color first_color second_color=case first_color of
    Color {red=first_red,green=first_green,blue=first_blue,alpha=first_alpha}->case second_color of
        Color {red=second_red,green=second_green,blue=second_blue,alpha=second_alpha}->Color {red=first_red*second_red,green=first_green*second_green,blue=first_blue*second_blue,alpha=first_alpha*second_alpha}

combine_arrange::ET.Has_call_stack=>Arrange->Arrange->Arrange
combine_arrange first_arrange second_arrange=case first_arrange of
    Arrange {point=first_point,matrix=first_matrix,color=first_color}->case second_arrange of
        Arrange {point=second_point,matrix=second_matrix,color=second_color}->case first_point of
            Point {x=first_point_x,y=first_point_y}->case second_point of
                Point {x=second_point_x,y=second_point_y}->case first_matrix of
                    Matrix {x=first_matrix_x,y=first_matrix_y,x_x=first_matrix_x_x,x_y=first_matrix_x_y,y_x=first_matrix_y_x,y_y=first_matrix_y_y}->case second_matrix of
                        Matrix {x=second_matrix_x,y=second_matrix_y,x_x=second_matrix_x_x,x_y=second_matrix_x_y,y_x=second_matrix_y_x,y_y=second_matrix_y_y}->Arrange {point=let new_x=second_point_x+second_matrix_x-first_point_x-first_matrix_x in let new_y=second_point_y+second_matrix_y-first_point_y-first_matrix_y in Point {x=first_point_x+first_matrix_x-second_matrix_x+first_matrix_x_x*new_x+first_matrix_x_y*new_y,y=first_point_y+first_matrix_y-second_matrix_y+first_matrix_y_x*new_x+first_matrix_y_y*new_y},matrix=Matrix {x=second_matrix_x,y=second_matrix_y,x_x=first_matrix_x_x*second_matrix_x_x+first_matrix_x_y*second_matrix_y_x,x_y=first_matrix_x_x*second_matrix_x_y+first_matrix_x_y*second_matrix_y_y,y_x=first_matrix_y_x*second_matrix_x_x+first_matrix_y_y*second_matrix_y_x,y_y=first_matrix_y_x*second_matrix_x_y+first_matrix_y_y*second_matrix_y_y},color=multiply_color first_color second_color}

quick_create_vertex::ET.Has_call_stack=>Color->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Vertex
quick_create_vertex color x y u v=case color of
    Color {red,green,blue,alpha}->Vertex {parameter_id=0,font_size=0,x=x,y=y,u=u,v=v,red=red,green=green,blue=blue,alpha=alpha}

quick_create_rectangle_vertex::ET.Has_call_stack=>Color->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->DS.Seq Vertex
quick_create_rectangle_vertex color left down right up min_u min_v max_u max_v=case color of
    Color {red,green,blue,alpha}->DS.singleton (Vertex {parameter_id=0,font_size=0,x=left,y=down,u=min_u,v=max_v,red=red,green=green,blue=blue,alpha=alpha}) DS.|> Vertex {parameter_id=0,font_size=0,x=right,y=down,u=max_u,v=max_v,red=red,green=green,blue=blue,alpha=alpha} DS.|> Vertex {parameter_id=0,font_size=0,x=right,y=up,u=max_u,v=min_v,red=red,green=green,blue=blue,alpha=alpha} DS.|> Vertex {parameter_id=0,font_size=0,x=left,y=up,u=min_u,v=min_v,red=red,green=green,blue=blue,alpha=alpha}

quick_create_rectangle_text_vertex::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->DS.Seq Vertex->DS.Seq Vertex
quick_create_rectangle_text_vertex red green blue alpha left down right up min_u min_v max_u max_v font_size vertex=vertex DS.|> Vertex {parameter_id=0,font_size=font_size,x=left,y=down,u=min_u,v=min_v,red=red,green=green,blue=blue,alpha=alpha} DS.|> Vertex {parameter_id=0,font_size=font_size,x=right,y=down,u=max_u,v=min_v,red=red,green=green,blue=blue,alpha=alpha} DS.|> Vertex {parameter_id=0,font_size=font_size,x=right,y=up,u=max_u,v=max_v,red=red,green=green,blue=blue,alpha=alpha} DS.|> Vertex {parameter_id=0,font_size=font_size,x=left,y=up,u=min_u,v=max_v,red=red,green=green,blue=blue,alpha=alpha}

quick_create_rectangle_index::ET.Has_call_stack=>DS.Seq DW.Word32
quick_create_rectangle_index=DS.singleton 0 DS.|> 1 DS.|> 2 DS.|> 0 DS.|> 2 DS.|> 3

to_extended::ET.Has_call_stack=>FCT.CFloat->Extended
to_extended number=Finite {number=number}

from_extended::ET.Has_call_stack=>Extended->FCT.CFloat
from_extended extended=case extended of
    Negative_infinity->0
    Finite {number}->number
    Positive_infinity->0

mebibyte::ET.Has_call_stack=>Num a=>a
mebibyte=1048576

nanosecond::ET.Has_call_stack=>Num a=>a
nanosecond=1000000000

millisecond::ET.Has_call_stack=>Num a=>a
millisecond=1000000

{-# INLINE get_store_widget #-}
{-# INLINE update_store_widget #-}
{-# INLINE update_group_visual #-}
{-# INLINE update_vector_visual #-}
{-# INLINE update_vector_widget #-}
{-# INLINE hosted_update_vector_widget #-}
{-# INLINE action_vector #-}
{-# INLINE update_group_widget #-}
{-# INLINE hosted_update_group_widget #-}
{-# INLINE update_coroutine_state #-}
{-# INLINE functor_update_coroutine_state #-}
{-# INLINE get_sdl_pipeline #-}
{-# INLINE update_shader_reference #-}
{-# INLINE widget_lookup #-}
{-# INLINE lock_visual #-}
{-# INLINE triple_reverse #-}
{-# INLINE monad_action_swap #-}
{-# INLINE vector_io_map #-}
{-# INLINE move_clip #-}
{-# INLINE multiply_color #-}
{-# INLINE combine_arrange #-}
{-# INLINE quick_create_vertex #-}
{-# INLINE quick_create_rectangle_vertex #-}
{-# INLINE quick_create_rectangle_text_vertex #-}
{-# INLINE quick_create_rectangle_index #-}
{-# INLINE to_extended #-}
{-# INLINE from_extended #-}
{-# INLINE mebibyte #-}
{-# INLINE nanosecond #-}
{-# INLINE millisecond #-}