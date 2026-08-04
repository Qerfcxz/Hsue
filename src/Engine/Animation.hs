{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Animation where

import Engine.Container
import Engine.Projection
import Engine.Selector
import Engine.Type
import qualified Data.Vector.Storable as DVS
import qualified Foreign.C.Types as FCT

step_animation::FCT.CFloat->Int->Selector Bool->Engine a b c d e->Engine a b c d e
step_animation time leaf_id selector engine=engine {leaf=intmap_update leaf_id (update_projection_object (selector_update (`step_animation_a` time) selector)) engine.leaf}

step_animation_a::Bool->FCT.CFloat->Widget a b c d e->Widget a b c d e
step_animation_a loop time widget=case widget of
    Visual {origin,matrix,red,green,blue,alpha,visual}->case visual of
        Animation {delay,moment,half_width,half_height,reciprocal_width,reciprocal_height,padding,width_number,height_number,album_number,album_id,count,index}->let (new_index,new_moment)=step_animation_b loop delay count index (moment+time) in Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=Animation {delay=delay,moment=new_moment,half_width=half_width,half_height=half_height,reciprocal_width=reciprocal_width,reciprocal_height=reciprocal_height,padding=padding,width_number=width_number,height_number=height_number,album_number=album_number,album_id=album_id,count=count,index=new_index}}
        _->widget
    _->widget

step_animation_b::Bool->DVS.Vector FCT.CFloat->Int->Int->FCT.CFloat->(Int,FCT.CFloat)
step_animation_b loop delay count index moment=let single_delay=delay DVS.! index in if moment<single_delay then (index,moment) else let new_moment=moment-single_delay in let new_index=index+1 in if count<=new_index then if loop then step_animation_b loop delay count 0 new_moment else (count-1,single_delay) else step_animation_b loop delay count new_index new_moment