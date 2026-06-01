{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Window where

import Engine.Other
import Engine.Type
import Engine.Widget
import qualified SDL.Function as F
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

remove_window::Int->Engine a->IO (Engine a)
remove_window window_id engine=let (maybe_window,new_window)=DIM.updateLookupWithKey (\_ _->Nothing) window_id engine.window in case maybe_window of
    Nothing->return engine
    Just (Window {sdl_window_id,sdl_window,window_bound})->do
        F.sdl_destroywindow sdl_window
        let target_bound=DIM.restrictKeys engine.bound window_bound
        DF.mapM_ (\bound->clean_resource bound.resource) target_bound
        return (engine {bound=DIM.withoutKeys engine.bound window_bound,node=DIM.foldlWithKey' (\node bound_id bound->remove_window_a bound_id bound.ancestry node) engine.node target_bound,window=new_window,window_map=map_delete sdl_window_id engine.window_map})

remove_window_a::Int->DS.Seq Int->DIM.IntMap (Node a)->DIM.IntMap (Node a)
remove_window_a bound_id ancestry engine_node=case ancestry of
    DS.Empty->engine_node
    _ DS.:|> node_id->DIM.alter (maybe_update (\node->node {bound_child=intset_delete bound_id node.bound_child})) node_id engine_node