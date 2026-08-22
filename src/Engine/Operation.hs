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
    _->EE.empty_error

update_store_widget::(Convert Data a,Convert a Data)=>(a->a)->Widget b c d e f->Widget b c d e f
update_store_widget update widget=case widget of
    Store {store}->Store {store=convert (update (convert store))}
    _->EE.empty_error

update_vector_widget::Int->(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
update_vector_widget this_index update widget=case widget of
    Vector {index,vector_widget}->Vector {index=index,vector_widget=CMST.runST (action_vector_widget (\this_vector_widget->DVM.write this_vector_widget this_index (update (vector_widget DV.! this_index))) vector_widget)}
    _->EE.empty_error

default_update_vector_widget::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
default_update_vector_widget update widget=case widget of
    Vector {index,vector_widget}->Vector {index=index,vector_widget=CMST.runST (action_vector_widget (\this_vector_widget->DVM.write this_vector_widget index (update (vector_widget DV.! index))) vector_widget)}
    _->EE.empty_error

action_vector_widget::DVM.PrimMonad a=>(DVM.MVector (DVM.PrimState a) (Widget b c d e f)->a ())->DV.Vector (Widget b c d e f)->a (DV.Vector (Widget b c d e f))
action_vector_widget action vector_widget=do
    new_vector_widget<-DV.thaw vector_widget
    action new_vector_widget
    DV.unsafeFreeze new_vector_widget

update_group_widget::Int->(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
update_group_widget this_index update widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=int_map_update this_index update group_widget}
    _->EE.empty_error

default_update_group_widget::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
default_update_group_widget update widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=int_map_update index update group_widget}
    _->EE.empty_error

get_sdl_pipeline::Pipeline->FP.Ptr SDLT.SDL_GPUGraphicsPipeline
get_sdl_pipeline pipeline=case pipeline of
    Pipeline {sdl_pipeline}->sdl_pipeline
    Default_pipeline {sdl_pipeline}->sdl_pipeline

update_shader_reference::(Int->Int)->Shader->Shader
update_shader_reference update shader=case shader of
    Shader {sdl_shader,reference}->Shader {sdl_shader=sdl_shader,reference=update reference}

widget_lookup::Widget a b c d e->Widget a b c d e
widget_lookup this_widget=case this_widget of
    Group {index,group_widget}->widget_lookup (int_map_lookup index group_widget)
    Vector {index,vector_widget}->widget_lookup (vector_widget DV.! index)
    Widget_trigger {widget}->widget_lookup widget
    Widget_io_trigger {widget}->widget_lookup widget
    Widget_mix_trigger {widget}->widget_lookup widget
    Coroutine {index,coroutine_state}->widget_lookup (int_map_lookup index coroutine_state).widget
    _->this_widget

lock_widget::Custom_widget d=>Widget a b c d e->Widget a b c d e
lock_widget widget=case widget of
    Visual {visual}->Visual {visual=lock_visual visual}
    Group_visual {arrange,collect_order,group_visual}->Group_visual {arrange=arrange,collect_order=collect_order,group_visual=fmap lock_visual group_visual}
    Vector_visual {arrange,collect_order,vector_visual,size}->Vector_visual {arrange=arrange,collect_order=collect_order,vector_visual=fmap lock_visual vector_visual,size=size}
    Custom_widget {custom}->Custom_widget {custom=custom_widget_lock custom}
    _->widget

lock_visual::Visual->Visual
lock_visual visual=case visual of
    Picture {arrange,half_width,half_height,min_u,min_v,max_u,max_v,path}->Picture {arrange=arrange,half_width=half_width,half_height=half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,path=path,locked=True}
    Atlas {arrange,clip_request,path,clip,index}->Atlas {arrange=arrange,clip_request=clip_request,path=path,clip=clip,index=index,locked=True}
    Text {arrange,half_width,half_height,current_y,min_y,max_y,article,charset}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=current_y,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=True}
    _->visual

{-# INLINE get_store_widget #-}
{-# INLINE update_store_widget #-}
{-# INLINE update_vector_widget #-}
{-# INLINE default_update_vector_widget #-}
{-# INLINE action_vector_widget #-}
{-# INLINE update_group_widget #-}
{-# INLINE default_update_group_widget #-}
{-# INLINE get_sdl_pipeline #-}
{-# INLINE update_shader_reference #-}
{-# INLINE widget_lookup #-}
{-# INLINE lock_widget #-}
{-# INLINE lock_visual #-}