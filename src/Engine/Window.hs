{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Window where

import Engine.Other
import Engine.Type
import qualified SDL.Function as F
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

remove_window::Int->Engine a->IO (Engine a)
remove_window window_id engine=let (new_window,Window {sdl_window_id,sdl_window,window_bound,triangle_graphics_pipeline})=intmap_delete_lookup window_id engine.window in do
    catch_false (F.sdl_waitforgpuidle engine.device)
    F.sdl_releasewindowfromgpudevice engine.device sdl_window
    F.sdl_releasegpugraphicspipeline engine.device triangle_graphics_pipeline
    F.sdl_destroywindow sdl_window
    return (engine {bound=DIM.withoutKeys engine.bound window_bound,node=DIM.foldlWithKey' (\this_node bound_id bound->remove_window_a bound_id bound.ancestry this_node) engine.node (DIM.restrictKeys engine.bound window_bound),window=new_window,window_map=map_delete sdl_window_id engine.window_map})

remove_window_a::Int->DS.Seq Int->DIM.IntMap (Node a)->DIM.IntMap (Node a)
remove_window_a bound_id ancestry node=case ancestry of
    DS.Empty->node
    _ DS.:|> node_id->intmap_update node_id (\this_node->this_node {bound_child=intset_delete bound_id this_node.bound_child}) node