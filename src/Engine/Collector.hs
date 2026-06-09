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
collect projection_path collect_id collect_strategy engine=engine {free=intmap_update collect_id (update_projection_free (projection_update_object (collect_a (DS.singleton (to_graph engine.u engine.v engine.atlas (path_lookup_projection_bound projection_path engine.bound))) collect_strategy))) engine.free}

collect_a::DS.Seq Graph->Collect_strategy->Widget a->Widget a
collect_a seq_graph collect_strategy widget=case widget of
    Collector {initial_min_index,initial_max_index,min_index,max_index,graph}->case collect_strategy of
        Min_collect_strategy->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index-1,max_index=max_index,graph=intmap_insert min_index seq_graph graph}
        Max_collect_strategy->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=max_index+1,graph=intmap_insert max_index seq_graph graph}
        Index_collect_strategy {seat}->if seat<=min_index then Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=seat-1,max_index=max_index,graph=intmap_insert seat seq_graph graph} else if max_index<=seat then Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=seat+1,graph=intmap_insert seat seq_graph graph} else Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=max_index,graph=intmap_insert seat seq_graph graph}
    _->error "collect_a: error 1"

to_graph::FCT.CFloat->FCT.CFloat->Atlas->Widget a->Graph
to_graph u v atlas widget=case widget of
    Visual {red,green,blue,alpha,matrix,visual}->case visual of
        Triangle {first_point,second_point,third_point}->let new_first_point=apply_matrix matrix first_point in let new_second_point=apply_matrix matrix second_point in let new_third_point=apply_matrix matrix third_point in Graph {vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_first_point.x,y=new_first_point.y,u=u,v=v}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_second_point.x,y=new_second_point.y,u=u,v=v} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_third_point.x,y=new_third_point.y,u=u,v=v},index=DS.singleton 0 DS.|> 1 DS.|> 2}
        Convex_polygon {point}->let vertex=fmap ((\this_point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=this_point.x,y=this_point.y,u=u,v=v}) . apply_matrix matrix) point in let number=DS.length point in if number<3 then error "to_graph: error 1" else Graph {vertex=vertex,index=DS.fromFunction (3*(number-2)) for_convex_polygon}
        Regular_polygon {number,center,radius,angle}->if number<3 then error "to_graph: error 2" else let new_angle=2*pi/fromIntegral number in Graph {vertex=fmap ((\point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=point.x,y=point.y,u=u,v=v}) . apply_matrix matrix) (DS.fromFunction number (\index->let direction=angle+fromIntegral index*new_angle in Point {x=center.x+radius*cos direction,y=center.y+radius*sin direction})),index=DS.fromFunction (3*(number-2)) for_convex_polygon}
        Picture {left,down,right,up,index}->case intmap_lookup index atlas.region of
            Region {min_u,min_v,max_u,max_v}->let first_point=apply_matrix matrix (Point {x=left,y=down}) in let second_point=apply_matrix matrix (Point {x=right,y=down}) in let third_point=apply_matrix matrix (Point {x=right,y=up}) in let fourth_point=apply_matrix matrix (Point {x=left,y=up}) in Graph {vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=first_point.x,y=first_point.y,u=min_u,v=max_v}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=second_point.x,y=second_point.y,u=max_u,v=max_v} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=third_point.x,y=third_point.y,u=max_u,v=min_v} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=fourth_point.x,y=fourth_point.y,u=min_u,v=min_v},index=DS.singleton 0 DS.|> 1 DS.|> 2 DS.|> 0 DS.|> 2 DS.|> 3}
    _->error "to_graph: error 3"

for_convex_polygon::Int->DW.Word32
for_convex_polygon index=let (quotient,remainder)=divMod index 3 in let new_quotient=fromIntegral quotient+1 in case remainder of
    0->0
    1->new_quotient
    2->new_quotient+1
    _->error "for_convex_polygon: error 1"

move::Projection_move->Int->Collect_strategy->Engine a->Engine a
move projection_move collect_id collect_strategy engine=let (free,widget)=move_update_lookup_projection_free projection_move consume_widget engine.free in engine {free=intmap_update collect_id (update_projection_free (projection_update_object (collect_a (move_a widget) collect_strategy))) free}

move_a::Widget a->DS.Seq Graph
move_a widget=case widget of
    Collector {graph}->DF.foldl' (DS.><) DS.empty graph
    _->error "move_a: error 1"

consume_widget::Widget a->Widget a
consume_widget widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=initial_min_index,max_index=initial_max_index,graph=DIM.empty}
    _->error "consume_widget: error 1"

for_submit::DIM.IntMap (DS.Seq Graph)->Graph
for_submit graph=let (vertex,index,_)=DIM.foldl' (DF.foldl' (flip for_submit_a)) (DS.empty,DS.empty,0) graph in Graph {vertex=vertex,index=index}

for_submit_a::Graph->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)
for_submit_a graph (this_vertex,this_index,offset)=case graph of
    Graph {vertex,index}->(this_vertex DS.>< vertex,this_index DS.>< fmap (+offset) index,offset+fromIntegral (DS.length vertex))