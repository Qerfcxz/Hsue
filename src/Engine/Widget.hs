{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Widget where

import Engine.Other
import Engine.Type
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

create_active::Maybe Int->Widget_request a->Int->Engine a->IO (Engine a)
create_active father widget_request active_id engine=do
    (widget,next)<-make_active widget_request
    case father of
        Nothing->return (engine {active=intmap_insert active_id (Active {next=next,ancestry=DS.empty,widget=widget}) engine.active})
        Just node_id->let (maybe_node,new_node)=DIM.updateLookupWithKey (\_->just_update (\node->node {active_child=intset_insert active_id node.active_child})) node_id engine.node in case maybe_node of
            Nothing->error "create_active: error 1"
            Just node->do
                return (engine {active=intmap_insert active_id (Active {next=next,ancestry=node.ancestry DS.|> node_id,widget=widget}) engine.active,node=new_node})

make_active::Widget_request a->IO (Widget a,Engine a->Event->Maybe Int)
make_active widget_request=case widget_request of
    Trigger_request {next,trigger}->return (Trigger {trigger=trigger},next)
    Io_trigger_request {next,io_trigger}->return (Io_trigger {io_trigger=io_trigger},next)

remove_active::Int->Engine a->IO (Engine a)
remove_active active_id engine=let (maybe_active,new_active)=DIM.updateLookupWithKey (\_ _->Nothing) active_id engine.active in case maybe_active of
    Nothing->error "remove_active: error 1"
    Just active->do
        clean_widget active.widget
        case active.ancestry of
            DS.Empty->return (engine {active=new_active})
            _ DS.:|> node_id->return (engine {active=new_active,node=DIM.alter (maybe_update (\node->node {active_child=intset_delete active_id node.active_child})) node_id engine.node})

remove_free::Int->Engine a->IO (Engine a)
remove_free free_id engine=let (maybe_free,new_free)=DIM.updateLookupWithKey (\_ _->Nothing) free_id engine.free in case maybe_free of
    Nothing->error "remove_free: error 1"
    Just free->do
        clean_widget free.widget
        case free.ancestry of
            DS.Empty->return (engine {free=new_free})
            _ DS.:|> node_id->return (engine {free=new_free,node=DIM.alter (maybe_update (\node->node {free_child=intset_delete free_id node.free_child})) node_id engine.node})

remove_bound::Int->Engine a->IO (Engine a)
remove_bound bound_id engine=let (maybe_bound,new_bound)=DIM.updateLookupWithKey (\_ _->Nothing) bound_id engine.bound in case maybe_bound of
    Nothing->error "remove_bound: error 1"
    Just bound->do
        clean_widget bound.widget
        let new_window=DIM.alter (maybe_update (\window->window {window_bound=intset_delete bound_id window.window_bound})) bound.window_id engine.window in case bound.ancestry of
            DS.Empty->return (engine {bound=new_bound,window=new_window})
            _ DS.:|> node_id->return (engine {bound=new_bound,node=DIM.alter (maybe_update (\node->node {bound_child=intset_delete bound_id node.bound_child})) node_id engine.node,window=new_window})

clean_widget::Widget a->IO ()
clean_widget widget=case widget of
    Trigger {}->return ()
    Io_trigger {}->return ()