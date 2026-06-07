{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Collector where

import Engine.Backup
import Engine.Other
import Engine.Type
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Word as DW

collect::Backup_path->Backup_path->Collect_strategy->Engine a->Engine a
collect from_backup_path to_backup_path collect_strategy engine=engine {free=update_free_backup to_backup_path (collect_a (DS.singleton (to_graph (lookup_bound_backup from_backup_path engine.bound))) collect_strategy) engine.free}

collect_a::DS.Seq Graph->Collect_strategy->Widget a->Widget a
collect_a new_graph collect_strategy widget=case widget of
    Collector {initial_min_index,initial_max_index,min_index,max_index,graph}->case collect_strategy of
        Min_collect->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index-1,max_index=max_index,graph=intmap_insert min_index new_graph graph}
        Max_collect->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=max_index+1,graph=intmap_insert max_index new_graph graph}
        Index_collect {seat}->if seat<=min_index then Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=seat-1,max_index=max_index,graph=intmap_insert seat new_graph graph} else if max_index<=seat then Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=seat+1,graph=intmap_insert seat new_graph graph} else Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=max_index,graph=intmap_insert seat new_graph graph}
    _->error "collect_a: error 1"

to_graph::Widget a->Graph
to_graph widget=case widget of
    Geometry {red,green,blue,alpha,matrix,geometry}->case geometry of
        Triangle {first_point,second_point,third_point}->let new_first_point=apply_matrix matrix first_point in let new_second_point=apply_matrix matrix second_point in let new_third_point=apply_matrix matrix third_point in Graph {vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_first_point.x,y=new_first_point.y}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_second_point.x,y=new_second_point.y} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_third_point.x,y=new_third_point.y},index=DS.singleton 0 DS.|> 1 DS.|> 2}
        Convex_polygon {point}->let vertex=fmap ((\this_point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=this_point.x,y=this_point.y}) . apply_matrix matrix) point in let number=DS.length point in if number<3 then error "to_graph: error 1" else Graph {vertex=vertex,index=DS.fromFunction (3*(number-2)) for_convex_polygon}
        Regular_polygon {number,center,radius,angle}->if number<3 then error "to_graph: error 2" else let new_angle=2*pi/fromIntegral number in Graph {vertex=fmap ((\point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=point.x,y=point.y}) . apply_matrix matrix) (DS.fromFunction number (\index->let direction=angle+fromIntegral index*new_angle in Point {x=center.x+radius*cos direction,y=center.y+radius*sin direction})),index=DS.fromFunction (3*(number-2)) for_convex_polygon}
    _->error "to_graph: error 3"

for_convex_polygon::Int->DW.Word32
for_convex_polygon index=let (quotient,remainder)=divMod index 3 in let new_quotient=fromIntegral quotient+1 in case remainder of
    0->0
    1->new_quotient
    2->new_quotient+1
    _->error "for_convex_polygon: error 1"

move::Backup_path->Backup_path->Move_strategy->Engine a->Engine a
move from_backup_path to_backup_path move_strategy engine=let (collect_strategy,consume)=to_collect_strategy move_strategy in let (free,widget)=consume_update_lookup_free_backup consume from_backup_path consume_widget engine.free in engine {free=update_free_backup to_backup_path (collect_a (move_a widget) collect_strategy) free}

move_a::Widget a->DS.Seq Graph
move_a widget=case widget of
    Collector {graph}->DF.foldl' (DS.><) DS.empty graph
    _->error "move_a: error 1"

to_collect_strategy::Move_strategy->(Collect_strategy,Bool)
to_collect_strategy move_strategy=case move_strategy of
    Min_move {consume}->(Min_collect,consume)
    Max_move {consume}->(Max_collect,consume)
    Index_move {consume,seat}->(Index_collect {seat},consume)

consume_widget::Widget a->Widget a
consume_widget widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=initial_min_index,max_index=initial_max_index,graph=DIM.empty}
    _->error "consume_widget: error 1"

for_submit::DIM.IntMap (DS.Seq Graph)->Graph
for_submit graph=let (vertex,index,_)=DIM.foldl' (DF.foldl' (flip for_submit_a)) (DS.empty,DS.empty,0) graph in Graph {vertex=vertex,index=index}

for_submit_a::Graph->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)
for_submit_a graph (this_vertex,this_index,offset)=case graph of
    Graph {vertex,index}->(this_vertex DS.>< vertex,this_index DS.>< fmap (+offset) index,offset+fromIntegral (DS.length vertex))