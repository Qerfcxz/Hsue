{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Animation where

import Engine.Helper
import Engine.Projection
import Engine.Type
import qualified Data.Vector.Storable as DVS
import qualified Foreign.C.Types as FCT

step_animation::Bool->FCT.CFloat->Engine a b c d e->Engine a b c d e
step_animation loop time engine=engine {leaf=fmap (update_projection_object (update_all_widget (step_animation_a loop time))) engine.leaf}

step_animation_a::Bool->FCT.CFloat->Widget a b c d e->Widget a b c d e
step_animation_a loop time widget=case widget of
    Visual {origin,matrix,red,green,blue,alpha,visual}->case visual of
        Animation {delay,moment,frame_width,frame_height,width,height,padding,width_number,height_number,album_number,album_id,count,index}->let (new_index,new_moment)=step_animation_b loop delay count index (moment+time) in Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=Animation {delay=delay,moment=new_moment,frame_width=frame_width,frame_height=frame_height,width=width,height=height,padding=padding,width_number=width_number,height_number=height_number,album_number=album_number,album_id=album_id,count=count,index=new_index}}
        _->widget
    _->widget

step_animation_b::Bool->DVS.Vector FCT.CFloat->Int->Int->FCT.CFloat->(Int,FCT.CFloat)
step_animation_b loop delay count index moment=let single_delay=delay DVS.! index in if moment<single_delay then (index,moment) else let new_moment=moment-single_delay in let new_index=index+1 in if count<=new_index then if loop then step_animation_b loop delay count 0 new_moment else (count-1,single_delay) else step_animation_b loop delay count new_index new_moment