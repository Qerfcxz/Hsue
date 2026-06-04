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
collect from_path to_path strategy engine=engine {free=update_free_backup to_path (collect_a (DS.singleton (to_graph (lookup_bound_backup from_path engine.bound))) strategy) engine.free}

collect_a::DS.Seq Graph->Collect_strategy->Widget a->Widget a
collect_a new_graph strategy widget=case widget of
    Collector {initial_min_index,initial_max_index,min_index,max_index,graph}->case strategy of
        Min_collect->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index-1,max_index=max_index,graph=intmap_insert min_index new_graph graph}
        Max_collect->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=max_index+1,graph=intmap_insert max_index new_graph graph}
        Index_collect {seat}->if seat<=min_index then Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=seat-1,max_index=max_index,graph=intmap_insert seat new_graph graph} else if max_index<=seat then Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=seat+1,graph=intmap_insert seat new_graph graph} else Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=min_index,max_index=max_index,graph=intmap_insert seat new_graph graph}
    _->error "collect_a: error 1"

to_graph::Widget a->Graph
to_graph widget=case widget of
    Geometry {red,green,blue,alpha,geometry}->case geometry of
        Triangle {first_point,second_point,third_point}->Graph {vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=first_point.x,y=first_point.y}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=second_point.x,y=second_point.y} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=third_point.x,y=third_point.y},index=DS.singleton 0 DS.|> 1 DS.|> 2}
        Convex_polygon {point}->let vertex=fmap (\this_point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=this_point.x,y=this_point.y}) point in let number=DS.length point in if number<3 then error "to_graph: error 1" else Graph {vertex=vertex,index=DS.fromFunction (3*(number-2)) for_convex_polygon}
        Regular_polygon {number,center,radius,angle}->if number<3 then error "to_graph: error 2" else let new_angle=2*pi/fromIntegral number in Graph {vertex=fmap (\this_point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=this_point.x,y=this_point.y}) (DS.fromFunction number (\index->let direction=angle+fromIntegral index*new_angle in Point {x=center.x+radius*cos direction,y=center.y+radius*sin direction})),index=DS.fromFunction (3*(number-2)) for_convex_polygon}
    _->error "to_graph: error 1"

for_convex_polygon::Int->DW.Word32
for_convex_polygon index=let (quotient,remainder)=divMod index 3 in let new_quotient=fromIntegral quotient+1 in case remainder of
    0->0
    1->new_quotient
    2->new_quotient+1
    _->error "for_convex_polygon: error 1"

move::Backup_path->Backup_path->Move_strategy->Engine a->Engine a
move from_path to_path strategy engine=let (new_strategy,consume)=to_collect_strategy strategy in let (new_free,new_widget)=whether_update_lookup_free_backup consume from_path consume_widget engine.free in engine {free=update_free_backup to_path (collect_a (move_a new_widget) new_strategy) new_free}


move_a::Widget a->DS.Seq Graph
move_a widget=case widget of
    Collector {graph}->DF.foldl' (DS.><) DS.empty graph
    _->error "move_a: error 1"

to_collect_strategy::Move_strategy->(Collect_strategy,Bool)
to_collect_strategy strategy=case strategy of
    Min_move {consume}->(Min_collect,consume)
    Max_move {consume}->(Max_collect,consume)
    Index_move {consume,seat}->(Index_collect {seat},consume)

consume_widget::Widget a->Widget a
consume_widget widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=initial_min_index,max_index=initial_max_index,graph=DIM.empty}
    _->error "consume_widget: error 1"

for_submit::DIM.IntMap (DS.Seq Graph)->Graph
for_submit graph=let (_,new_vertex,new_index)=DIM.foldl' (DF.foldl' (\(offset,this_vertex,this_index) (Graph {vertex,index})->(offset+fromIntegral (DS.length vertex),this_vertex DS.>< vertex,this_index DS.>< fmap (+offset) index))) (0,DS.empty,DS.empty) graph in Graph {vertex=new_vertex,index=new_index}