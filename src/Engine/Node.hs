{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Node where

import Engine.Other
import Engine.Type
import Engine.Widget
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS

create_node::Maybe Int->(Engine a->Event->Event)->(Engine a->Request a->Widget a->Widget a)->Int->Engine a->Engine a
create_node father event_transform widget_transform node_id engine=case father of
    Nothing->engine {node=intmap_insert node_id (Node {active_child=DIS.empty,free_child=DIS.empty,bound_child=DIS.empty,node_child=DIS.empty,ancestry=DS.empty,event_transform=event_transform,widget_transform=widget_transform}) engine.node}
    Just new_node_id->let (maybe_node,new_node)=DIM.updateLookupWithKey (\_->just_update (\node->node {node_child=intset_insert node_id node.node_child})) new_node_id engine.node in case maybe_node of
        Nothing->error "create_node: error 1"
        Just node->engine {node=intmap_insert node_id (Node {active_child=DIS.empty,free_child=DIS.empty,bound_child=DIS.empty,node_child=DIS.empty,ancestry=node.ancestry DS.|> new_node_id,event_transform=event_transform,widget_transform=widget_transform}) new_node}

remove_node::Int->Engine a->IO (Engine a)
remove_node node_id engine=let (maybe_node,new_node)=DIM.updateLookupWithKey (\_ _->Nothing) node_id engine.node in case maybe_node of
    Nothing->error "remove_node: error 1"
    Just node->case node.ancestry of
        DS.Empty->remove_node_a node.active_child node.free_child node.bound_child node.node_child (engine {node=new_node})
        _ DS.:|> new_node_id->remove_node_a node.active_child node.free_child node.bound_child node.node_child (engine {node=DIM.alter (maybe_update (\this_node->this_node {node_child=intset_delete node_id this_node.node_child})) new_node_id new_node})

remove_node_a::DIS.IntSet->DIS.IntSet->DIS.IntSet->DIS.IntSet->Engine a->IO (Engine a)
remove_node_a active_child free_child bound_child node_child engine=do
    new_engine<-intset_foldm remove_node_bound bound_child (DIS.foldl' (flip remove_node_free) (DIS.foldl' (flip remove_node_active) engine active_child) free_child)
    intset_foldm remove_node_node node_child new_engine

remove_node_active::Int->Engine a->Engine a
remove_node_active active_id engine=engine {active=intmap_delete active_id engine.active}

remove_node_free::Int->Engine a->Engine a
remove_node_free free_id engine=engine {free=intmap_delete free_id engine.free}

remove_node_bound::Int->Engine a->IO (Engine a)
remove_node_bound bound_id engine=let (maybe_bound,new_bound)=DIM.updateLookupWithKey (\_ _->Nothing) bound_id engine.bound in case maybe_bound of
    Nothing->error "remove_node_bound: error 1"
    Just bound->do
        clean_resource bound.resource
        return (engine {bound=new_bound,window=DIM.alter (maybe_update (\window->window {window_bound=intset_delete bound_id window.window_bound})) bound.window_id engine.window})

remove_node_node::Int->Engine a->IO (Engine a)
remove_node_node node_id engine=case DIM.lookup node_id engine.node of
    Nothing->error "remove_node_node: error 1"
    Just node->remove_node_a node.active_child node.free_child node.bound_child node.node_child (engine {node=intmap_delete node_id engine.node})