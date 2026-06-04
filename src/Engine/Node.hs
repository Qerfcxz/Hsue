{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Node where

import Engine.Other
import Engine.Type
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS

create_node::Maybe Int->(Engine a->Event->Event)->(Engine a->Widget a->Widget a)->Int->Engine a->Engine a
create_node father event_transform widget_transform node_id engine=case father of
    Nothing->engine {node=intmap_insert node_id (Node {active_child=DIS.empty,free_child=DIS.empty,bound_child=DIS.empty,node_child=DIS.empty,ancestry=DS.empty,event_transform=event_transform,widget_transform=widget_transform}) engine.node}
    Just new_node_id->let (new_node,node)=intmap_update_lookup new_node_id (\this_node->this_node {node_child=intset_insert node_id this_node.node_child}) engine.node in engine {node=intmap_insert node_id (Node {active_child=DIS.empty,free_child=DIS.empty,bound_child=DIS.empty,node_child=DIS.empty,ancestry=node.ancestry DS.|> new_node_id,event_transform=event_transform,widget_transform=widget_transform}) new_node}

remove_node::Int->Engine a->Engine a
remove_node node_id engine=let (new_node,node)=intmap_delete_lookup node_id engine.node in case node.ancestry of
    DS.Empty->remove_node_a node.active_child node.free_child node.bound_child node.node_child (engine {node=new_node})
    _ DS.:|> new_node_id->remove_node_a node.active_child node.free_child node.bound_child node.node_child (engine {node=intmap_update new_node_id (\this_node->this_node {node_child=intset_delete node_id this_node.node_child}) new_node})

remove_node_a::DIS.IntSet->DIS.IntSet->DIS.IntSet->DIS.IntSet->Engine a->Engine a
remove_node_a active_child free_child bound_child node_child engine=DIS.foldl' (flip remove_node_node) (DIS.foldl' (flip remove_node_bound) (DIS.foldl' (flip remove_node_free) (DIS.foldl' (flip remove_node_active) engine active_child) free_child) bound_child) node_child

remove_node_active::Int->Engine a->Engine a
remove_node_active active_id engine=engine {active=intmap_delete active_id engine.active}

remove_node_free::Int->Engine a->Engine a
remove_node_free free_id engine=engine {free=intmap_delete free_id engine.free}

remove_node_bound::Int->Engine a->Engine a
remove_node_bound bound_id engine=let (new_bound,bound)=intmap_delete_lookup bound_id engine.bound in engine {bound=new_bound,window=intmap_update bound.window_id (\window->window {window_bound=intset_delete bound_id window.window_bound}) engine.window}

remove_node_node::Int->Engine a->Engine a
remove_node_node node_id engine=let node=intmap_lookup node_id engine.node in remove_node_a node.active_child node.free_child node.bound_child node.node_child (engine {node=intmap_delete node_id engine.node})