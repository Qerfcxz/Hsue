{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Engine.Type where

import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS

data Engine a=Engine {state::a,leaf::DIM.IntMap (Leaf a),node::DIM.IntMap (Node a)}

data Leaf a=Leaf {ancestor::Maybe Int,father::Maybe Int,next::Engine a->Event->Maybe Int,widget::Widget a}

data Node a=Node {ancestor::Maybe Int,father::Maybe Int,leaf_child::DIS.IntSet,node_child::DIS.IntSet,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}

data Widget a=Trigger {trigger::Event->Engine a->Engine a}|Io_trigger {io_trigger::Event->Engine a->IO (Engine a)}

data Event=Unknown|Quit|Time