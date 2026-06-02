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

collect::Int->Int->Backup_strategy->Backup_strategy->Collect_strategy->Engine a->Engine a
collect bound_id collector_id bound_strategy collector_strategy strategy engine=engine {free=intmap_update collector_id (\(Free {ancestry,backup})->Free {ancestry=ancestry,backup=backup_update collector_strategy (collect_a (DS.singleton (to_graph (backup_lookup bound_strategy (intmap_lookup bound_id engine.bound).backup))) strategy) backup}) engine.free}

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
        Triangle {first_x,first_y,second_x,second_y,third_x,third_y}->Graph {vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=first_x,y=first_y}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=second_x,y=second_y} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=third_x,y=third_y},index=DS.singleton 0 DS.|> 1 DS.|> 2}
    _->error "to_graph: error 1"

move::Int->Int->Backup_strategy->Backup_strategy->Move_strategy->Engine a->Engine a
move collector_id new_collector_id collector_strategy new_collector_strategy strategy engine=let (new_strategy,consume)=to_collect_strategy strategy in let (new_free,new_widget)=intmap_calculate collector_id (\Free {ancestry,backup}->let (new_backup,widget)=backup_update_lookup_whether consume collector_strategy consume_widget backup in (Free {ancestry=ancestry,backup=new_backup},widget)) engine.free in engine {free=intmap_update new_collector_id (\Free {ancestry,backup}->Free {ancestry=ancestry,backup=backup_update new_collector_strategy (collect_a (move_a new_widget) new_strategy) backup}) new_free}

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