{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Node where

import Engine.Container
import Engine.Leaf
import Engine.Projection
import Engine.Type
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS

create_node::Int->Maybe Int->(Engine a->Event->Event)->(Engine a->Widget a->Widget a)->Engine a->Engine a
create_node node_id maybe_father_id event_transform widget_transform engine=case maybe_father_id of
    Nothing->engine {node=intmap_insert node_id (Node {ancestry_id=DS.empty,leaf_child=DIS.empty,node_child=DIS.empty,event_transform=event_transform,widget_transform=widget_transform}) engine.node}
    Just father_id->let (node,single_node)=intmap_update_lookup father_id (\this_node->this_node {node_child=intset_insert node_id this_node.node_child}) engine.node in engine {node=intmap_insert node_id (Node {ancestry_id=single_node.ancestry_id DS.|> father_id,leaf_child=DIS.empty,node_child=DIS.empty,event_transform=event_transform,widget_transform=widget_transform}) node}

remove_node::Int->Engine a->IO (Engine a)
remove_node node_id engine=let (node,single_node)=intmap_delete_lookup node_id engine.node in case single_node.ancestry_id of
    DS.Empty->remove_node_a single_node.leaf_child single_node.node_child (engine {node=node})
    _ DS.:|> father_id->remove_node_a single_node.leaf_child single_node.node_child (engine {node=intmap_update father_id (\this_node->this_node {node_child=intset_delete node_id this_node.node_child}) node})

remove_node_a::DIS.IntSet->DIS.IntSet->Engine a->IO (Engine a)
remove_node_a leaf_child node_child engine=do
    new_engine<-intset_monad_fold remove_node_leaf leaf_child engine
    intset_monad_fold remove_node_node node_child new_engine

remove_node_leaf::Int->Engine a->IO (Engine a)
remove_node_leaf leaf_id engine=let (leaf,projection)=intmap_delete_lookup leaf_id engine.leaf in remove_widget (lookup_projection_object projection) (engine {leaf=leaf})

remove_node_node::Int->Engine a->IO (Engine a)
remove_node_node node_id engine=let (node,single_node)=intmap_delete_lookup node_id engine.node in remove_node_a single_node.leaf_child single_node.node_child (engine {node=node})