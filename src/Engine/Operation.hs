{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Operation where

import Engine.Container
import Engine.Type
import qualified SDL.Type as SDLT
import qualified Error.Error as EE
import qualified Control.Monad.ST as CMST
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Foreign.Ptr as FP

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

get_sdl_pipeline::Pipeline->FP.Ptr SDLT.SDL_GPUGraphicsPipeline
get_sdl_pipeline pipeline=case pipeline of
    Pipeline {sdl_pipeline}->sdl_pipeline
    Default_pipeline {sdl_pipeline}->sdl_pipeline

widget_lookup::Widget a b c d e->Widget a b c d e
widget_lookup this_widget=case this_widget of
    Group {index,group_widget}->widget_lookup (intmap_lookup index group_widget)
    Vector {index,vector_widget}->widget_lookup (vector_widget DV.! index)
    Widget_trigger {widget}->widget_lookup widget
    Widget_io_trigger {widget}->widget_lookup widget
    Widget_mix_trigger {widget}->widget_lookup widget
    Coroutine {index,coroutine_state}->widget_lookup (intmap_lookup index coroutine_state).widget
    _->this_widget

lock_canvas_widget::Widget a b c d e->Widget a b c d e
lock_canvas_widget widget=case widget of
    Visual {visual}->Visual {visual=lock_canvas_visual visual}
    Group_visual {arrange,collect_order,group_visual}->Group_visual {arrange=arrange,collect_order=collect_order,group_visual=fmap lock_canvas_visual group_visual}
    Vector_visual {arrange,collect_order,size,vector_visual}->Vector_visual {arrange=arrange,collect_order=collect_order,size=size,vector_visual=fmap lock_canvas_visual vector_visual}
    _->EE.quick_error "lock_canvas_widget" 0

lock_canvas_visual::Visual->Visual
lock_canvas_visual visual=case visual of
    Canvas {arrange,canvas_width,canvas_height,half_width,half_height,canvas_id}->Canvas {arrange=arrange,canvas_width=canvas_width,canvas_height=canvas_height,half_width=half_width,half_height=half_height,canvas_id=canvas_id,locked=True}
    _->visual

lock_widget::Custom_widget d=>Widget a b c d e->Widget a b c d e
lock_widget widget=case widget of
    Visual {visual}->Visual {visual=lock_visual visual}
    Group_visual {arrange,collect_order,group_visual}->Group_visual {arrange=arrange,collect_order=collect_order,group_visual=fmap lock_visual group_visual}
    Vector_visual {arrange,collect_order,size,vector_visual}->Vector_visual {arrange=arrange,collect_order=collect_order,size=size,vector_visual=fmap lock_visual vector_visual}
    Custom_widget {custom}->Custom_widget {custom=custom_widget_lock custom}
    _->widget

lock_visual::Visual->Visual
lock_visual visual=case visual of
    Picture {arrange,half_width,half_height,min_u,min_v,max_u,max_v,path}->Picture {arrange=arrange,half_width=half_width,half_height=half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,path=path,locked=True}
    Atlas {arrange,clip_request,path,clip,index}->Atlas {arrange=arrange,clip_request=clip_request,path=path,clip=clip,index=index,locked=True}
    Text {arrange,half_width,half_height,current_y,min_y,max_y,article,charset}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=current_y,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=True}
    _->visual

vector_io_map::Int->(Int->a->b->IO (b,c))->DV.Vector a->DVM.IOVector c->b->IO b
vector_io_map index action first_vector second_vector value=if index<0 then return value else do
    (new_value,new_new_value)<-action index (first_vector DV.! index) value
    DVM.unsafeWrite second_vector index new_new_value
    vector_io_map (index-1) action first_vector second_vector new_value

combine_arrange::Arrange->Arrange->Arrange
combine_arrange first_arrange second_arrange=case first_arrange of
    Arrange {point=first_point,matrix=first_matrix,red=first_red,green=first_green,blue=first_blue,alpha=first_alpha}->case second_arrange of
        Arrange {point=second_point,matrix=second_matrix,red=second_red,green=second_green,blue=second_blue,alpha=second_alpha}->case first_point of
            Point {x=first_point_x,y=first_point_y}->case second_point of
                Point {x=second_point_x,y=second_point_y}->case first_matrix of
                    Matrix {x=first_matrix_x,y=first_matrix_y,x_x=first_matrix_x_x,x_y=first_matrix_x_y,y_x=first_matrix_y_x,y_y=first_matrix_y_y}->case second_matrix of
                        Matrix {x=second_matrix_x,y=second_matrix_y,x_x=second_matrix_x_x,x_y=second_matrix_x_y,y_x=second_matrix_y_x,y_y=second_matrix_y_y}->Arrange {point=let x=second_point_x-first_point_x in let y=second_point_y-first_point_y in Point {x=first_point_x+first_matrix_x+first_matrix_x_x*x+first_matrix_x_y*y,y=first_point_y+first_matrix_y+first_matrix_y_x*x+first_matrix_y_y*y},matrix=Matrix {x=first_matrix_x_x*second_matrix_x+first_matrix_x_y*second_matrix_y+first_matrix_x,y=first_matrix_y_x*second_matrix_x+first_matrix_y_y*second_matrix_y+first_matrix_y,x_x=first_matrix_x_x*second_matrix_x_x+first_matrix_x_y*second_matrix_y_x,x_y=first_matrix_x_x*second_matrix_x_y+first_matrix_x_y*second_matrix_y_y,y_x=first_matrix_y_x*second_matrix_x_x+first_matrix_y_y*second_matrix_y_x,y_y=first_matrix_y_x*second_matrix_x_y+first_matrix_y_y*second_matrix_y_y},red=first_red*second_red,green=first_green*second_green,blue=first_blue*second_blue,alpha=first_alpha*second_alpha}

move_clip::Point->Clip->Clip
move_clip point clip=case clip of
    Clip {x,y,half_width,half_height,min_u,min_v,max_u,max_v}->Clip {x=x+point.x,y=y+point.y,half_width=half_width,half_height=half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v}