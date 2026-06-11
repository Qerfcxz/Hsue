{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Collector where

import Engine.Other
import Engine.Projection
import Engine.Type
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT

collect::Projection_path->Int->Collect_strategy->Engine a->Engine a
collect projection_path inactive_id collect_strategy engine=engine {inactive=intmap_update inactive_id (update_inactive_projection (update_object (collect_a (DS.singleton (to_submit engine.u engine.v engine.atlas (lookup_inactive_widget projection_path engine.inactive))) collect_strategy))) engine.inactive}

collect_a::DS.Seq Submit->Collect_strategy->Widget a->Widget a
collect_a seq_submit collect_strategy widget=case widget of
    Collector {initial_min_index,initial_max_index,min_index,max_index,submit}->case collect_strategy of
        Min_collect_strategy->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index-1,max_index=max_index,submit=intmap_insert min_index seq_submit submit}
        Max_collect_strategy->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=max_index+1,submit=intmap_insert max_index seq_submit submit}
        Index_collect_strategy {seat}->if seat<=min_index then Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=seat-1,max_index=max_index,submit=intmap_insert seat seq_submit submit} else if max_index<=seat then Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=seat+1,submit=intmap_insert seat seq_submit submit} else Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=max_index,submit=intmap_insert seat seq_submit submit}
    _->error "collect_a: error 1"

to_submit::FCT.CFloat->FCT.CFloat->Atlas->Widget a->Submit
to_submit u v atlas widget=case widget of
    Visual {red,green,blue,alpha,matrix,visual}->case visual of
        Triangle {first_point,second_point,third_point}->let new_first_point=apply_matrix matrix first_point in let new_second_point=apply_matrix matrix second_point in let new_third_point=apply_matrix matrix third_point in Submit {maybe_album_id=Nothing,vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_first_point.x,y=new_first_point.y,u=u,v=v}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_second_point.x,y=new_second_point.y,u=u,v=v} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_third_point.x,y=new_third_point.y,u=u,v=v},index=DS.singleton 0 DS.|> 1 DS.|> 2,vertex_length=3,index_length=3}
        Convex_polygon {point}->let vertex=fmap ((\this_point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=this_point.x,y=this_point.y,u=u,v=v}) . apply_matrix matrix) point in let number=DS.length point in if number<3 then error "to_submit: error 1" else let new_number=3*(number-2) in Submit {maybe_album_id=Nothing,vertex=vertex,index=DS.fromFunction new_number for_convex_polygon,vertex_length=fromIntegral number,index_length=fromIntegral new_number}
        Regular_polygon {number,center,radius,angle}->if number<3 then error "to_submit: error 2" else let new_number=3*(number-2) in let new_angle=2*pi/fromIntegral number in Submit {maybe_album_id=Nothing,vertex=fmap ((\point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=point.x,y=point.y,u=u,v=v}) . apply_matrix matrix) (DS.fromFunction number (\index->let direction=angle+fromIntegral index*new_angle in Point {x=center.x+radius*cos direction,y=center.y+radius*sin direction})),index=DS.fromFunction (3*(number-2)) for_convex_polygon,vertex_length=fromIntegral number,index_length=fromIntegral new_number}
        Picture {left,down,right,up,atlas_id}->case intmap_lookup atlas_id atlas.region of
            Region {min_u,min_v,max_u,max_v}->let first_point=apply_matrix matrix (Point {x=left,y=down}) in let second_point=apply_matrix matrix (Point {x=right,y=down}) in let third_point=apply_matrix matrix (Point {x=right,y=up}) in let fourth_point=apply_matrix matrix (Point {x=left,y=up}) in Submit {maybe_album_id=Nothing,vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=first_point.x,y=first_point.y,u=min_u,v=max_v}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=second_point.x,y=second_point.y,u=max_u,v=max_v} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=third_point.x,y=third_point.y,u=max_u,v=min_v} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=fourth_point.x,y=fourth_point.y,u=min_u,v=min_v},index=DS.singleton 0 DS.|> 1 DS.|> 2 DS.|> 0 DS.|> 2 DS.|> 3,vertex_length=4,index_length=6}
        Large_picture {left,down,right,up,album_id}->let first_point=apply_matrix matrix (Point {x=left,y=down}) in let second_point=apply_matrix matrix (Point {x=right,y=down}) in let third_point=apply_matrix matrix (Point {x=right,y=up}) in let fourth_point=apply_matrix matrix (Point {x=left,y=up}) in Submit {maybe_album_id=Just album_id,vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=first_point.x,y=first_point.y,u=0,v=1}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=second_point.x,y=second_point.y,u=1,v=1} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=third_point.x,y=third_point.y,u=1,v=0} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=fourth_point.x,y=fourth_point.y,u=0,v=0},index=DS.singleton 0 DS.|> 1 DS.|> 2 DS.|> 0 DS.|> 2 DS.|> 3,vertex_length=4,index_length=6}
        _->error "to_submit: error 3"
    _->error "to_submit: error 4"

for_convex_polygon::Int->DW.Word32
for_convex_polygon index=let (quotient,remainder)=divMod index 3 in let new_quotient=fromIntegral quotient+1 in case remainder of
    0->0
    1->new_quotient
    2->new_quotient+1
    _->error "for_convex_polygon: error 1"

move::Projection_move->Int->Collect_strategy->Engine a->Engine a
move projection_move inactive_id collect_strategy engine=let (inactive,widget)=update_lookup_inactive_object projection_move consume_widget engine.inactive in engine {inactive=intmap_update inactive_id (update_inactive_projection (update_object (collect_a (move_a widget) collect_strategy))) inactive}

move_a::Widget a->DS.Seq Submit
move_a widget=case widget of
    Collector {submit}->DF.foldl' (DS.><) DS.empty submit
    _->error "move_a: error 1"

consume_widget::Widget a->Widget a
consume_widget widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=initial_min_index,max_index=initial_max_index,submit=DIM.empty}
    _->error "consume_widget: error 1"

for_submit::DIM.IntMap (DS.Seq Submit)->(DS.Seq Vertex,DS.Seq DW.Word32,DS.Seq (Maybe Int,DW.Word32,DW.Word32))
for_submit submit=let (vertex,index,draw_call,_,_)=DIM.foldl' (DF.foldl' (flip for_submit_a)) (DS.empty,DS.empty,DS.empty,0,0) submit in (vertex,index,draw_call)

for_submit_a::Submit->(DS.Seq Vertex,DS.Seq DW.Word32,DS.Seq (Maybe Int,DW.Word32,DW.Word32),DW.Word32,DW.Word32)->(DS.Seq Vertex,DS.Seq DW.Word32,DS.Seq (Maybe Int,DW.Word32,DW.Word32),DW.Word32,DW.Word32)
for_submit_a submit (this_vertex,this_index,draw_call,vertex_offset,index_offset)=case submit of
    Submit {maybe_album_id,vertex,index,vertex_length,index_length}->case draw_call of
        DS.Empty->(this_vertex DS.>< vertex,this_index DS.>< fmap (vertex_offset+) index,DS.singleton (maybe_album_id,index_length,index_offset),vertex_offset+vertex_length,index_offset+index_length)
        new_draw_call DS.:|> (new_maybe_album_id,new_index_length,new_index_offset)->if maybe_album_id==new_maybe_album_id then (this_vertex DS.>< vertex,this_index DS.>< fmap (+vertex_offset) index,new_draw_call DS.|> (maybe_album_id,new_index_length+index_length,new_index_offset),vertex_offset+vertex_length,index_offset+index_length) else (this_vertex DS.>< vertex,this_index DS.>< fmap (+vertex_offset) index,draw_call DS.|> (maybe_album_id,index_length,index_offset),vertex_offset+vertex_length,index_offset+index_length)