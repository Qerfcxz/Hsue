{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Widget where

import Engine.Other
import Engine.Type
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

create_active::Maybe Int->Widget_request a->Int->Engine a->Engine a
create_active father widget_request active_id engine=let (widget,next)=make_active widget_request in case father of
    Nothing->engine {active=intmap_insert active_id (Active {next=next,ancestry=DS.empty,backup=Single widget}) engine.active}
    Just node_id->let (new_node,node)=intmap_update_lookup node_id (\this_node->this_node {active_child=intset_insert active_id this_node.active_child}) engine.node in engine {active=intmap_insert active_id (Active {next=next,ancestry=node.ancestry DS.|> node_id,backup=Single widget}) engine.active,node=new_node}

make_active::Widget_request a->(Widget a,Engine a->Event->Maybe Int)
make_active widget_request=case widget_request of
    Trigger_request {next,trigger}->(Trigger {trigger=trigger},next)
    Io_trigger_request {next,io_trigger}->(Io_trigger {io_trigger=io_trigger},next)
    _->error "make_active: error 1"

create_free::Maybe Int->Widget_request a->Int->Engine a->Engine a
create_free father widget_request free_id engine=let widget=make_free widget_request in case father of
    Nothing->engine {free=intmap_insert free_id (Free {ancestry=DS.empty,backup=Single widget}) engine.free}
    Just node_id->let (new_node,node)=intmap_update_lookup node_id (\this_node->this_node {free_child=intset_insert free_id this_node.free_child}) engine.node in engine {free=intmap_insert free_id (Free {ancestry=node.ancestry DS.|> node_id,backup=Single widget}) engine.free,node=new_node}

make_free::Widget_request a->Widget a
make_free widget_request=case widget_request of
    Collector_request {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=initial_min_index,max_index=initial_max_index,graph=DIM.empty}
    _->error "make_free: error 1"

create_bound::Maybe Int->Widget_request a->Int->Engine a->Engine a
create_bound father widget_request bound_id engine=let (widget,window_id)=make_bound widget_request in let new_window=intmap_update window_id (\window->window {window_bound=intset_insert bound_id window.window_bound}) engine.window in case father of
    Nothing->engine {bound=intmap_insert bound_id (Bound {window_id=window_id,ancestry=DS.empty,backup=Single widget}) engine.bound,window=new_window}
    Just node_id->let (new_node,node)=intmap_update_lookup node_id (\this_node->this_node {bound_child=intset_insert bound_id this_node.bound_child}) engine.node in engine {bound=intmap_insert bound_id (Bound {window_id=window_id,ancestry=node.ancestry DS.|> node_id,backup=Single widget}) engine.bound,node=new_node,window=new_window}

make_bound::Widget_request a->(Widget a,Int)
make_bound widget_request=case widget_request of
    Geometry_request {window_id,red,green,blue,alpha,geometry}->(Geometry {red,green,blue,alpha,geometry},window_id)
    _->error "make_bound: error 1"

remove_active::Int->Engine a->Engine a
remove_active active_id engine=let (new_active,active)=intmap_delete_lookup active_id engine.active in case active.ancestry of
    DS.Empty->engine {active=new_active}
    _ DS.:|> node_id->engine {active=new_active,node=intmap_update node_id (\node->node {active_child=intset_delete active_id node.active_child}) engine.node}

remove_free::Int->Engine a->Engine a
remove_free free_id engine=let (new_free,free)=intmap_delete_lookup free_id engine.free in case free.ancestry of
    DS.Empty->engine {free=new_free}
    _ DS.:|> node_id->engine {free=new_free,node=intmap_update node_id (\node->node {free_child=intset_delete free_id node.free_child}) engine.node}

remove_bound::Int->Engine a->Engine a
remove_bound bound_id engine=let (new_bound,bound)=intmap_delete_lookup bound_id engine.bound in let new_window=intmap_update bound.window_id (\window->window {window_bound=intset_delete bound_id window.window_bound}) engine.window in case bound.ancestry of
    DS.Empty->engine {bound=new_bound,window=new_window}
    _ DS.:|> node_id->engine {bound=new_bound,node=intmap_update node_id (\node->node {bound_child=intset_delete bound_id node.bound_child}) engine.node,window=new_window}