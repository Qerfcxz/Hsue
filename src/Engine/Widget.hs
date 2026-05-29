{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Widget where

import Engine.Other
import Engine.Type
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS

remove_bound::Int->Engine a->IO (Engine a)
remove_bound bound_id engine=let (maybe_bound,new_bound)=DIM.updateLookupWithKey (\_ _->Nothing) bound_id engine.bound in case maybe_bound of
    Nothing->error "remove_bound: error 1"
    Just bound->do
        remove_widget bound.widget
        let new_window=DIM.alter (maybe_update (\window->window {window_bound=DIS.delete bound_id window.window_bound})) bound.window_id engine.window in case bound.ancestry of
            DS.Empty->return (engine {bound=new_bound,window=new_window})
            _ DS.:|> node_id->return (engine {bound=new_bound,node=DIM.alter (maybe_update (\node->node {bound_child=DIS.delete bound_id node.bound_child})) node_id engine.node,window=new_window})

remove_widget::Widget a->IO ()
remove_widget widget=case widget of
    Trigger {}->return ()
    Io_trigger {}->return ()