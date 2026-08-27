{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Animation where

import Engine.Container
import Engine.Projection
import Engine.Selector
import Engine.Type
import qualified Error.Type as ET
import qualified Data.Vector.Storable as DVS
import qualified Foreign.C.Types as FCT

step_animation::ET.Has_call_stack=>FCT.CFloat->Int->Selector Bool->Engine a->Engine a
step_animation time leaf_id selector engine=engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (`step_animation_a` time) selector)) engine.leaf}

step_animation_a::ET.Has_call_stack=>Bool->FCT.CFloat->Widget a->Widget a
step_animation_a loop time widget=case widget of
    Visual {visual}->Visual {visual=step_animation_visual loop time visual}
    Group_visual {arrange,collect_order,group_visual}->Group_visual {arrange=arrange,collect_order=collect_order,group_visual=fmap (step_animation_visual loop time) group_visual}
    Vector_visual {arrange,collect_order,vector_visual,size}->Vector_visual {arrange=arrange,collect_order=collect_order,vector_visual=fmap (step_animation_visual loop time) vector_visual,size=size}
    _->widget

step_animation_visual::ET.Has_call_stack=>Bool->FCT.CFloat->Visual a->Visual a
step_animation_visual loop time visual=case visual of
    Animation {arrange,delay,moment,half_width,half_height,padding,exponent_width,exponent_height,width_number,height_number,album_number,album_id,count,index}->let (new_index,new_moment)=step_animation_b loop delay count index (moment+time) in Animation {arrange=arrange,delay=delay,moment=new_moment,half_width=half_width,half_height=half_height,padding=padding,exponent_width=exponent_width,exponent_height=exponent_height,width_number=width_number,height_number=height_number,album_number=album_number,album_id=album_id,count=count,index=new_index}
    _->visual

step_animation_b::ET.Has_call_stack=>Bool->DVS.Vector FCT.CFloat->Int->Int->FCT.CFloat->(Int,FCT.CFloat)
step_animation_b loop delay count index moment=let single_delay=delay DVS.! index in if moment<single_delay then (index,moment) else let new_moment=moment-single_delay in let new_index=index+1 in if count<=new_index then if loop then step_animation_b loop delay count 0 new_moment else (count-1,single_delay) else step_animation_b loop delay count new_index new_moment

{-# INLINE step_animation #-}