module Engine.Other where

import qualified Control.Monad as CM
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU

catch_error::IO FCT.CBool->IO ()
catch_error io_value=do
    value<-io_value
    CM.unless (FMU.toBool value) (error "catch_error: error 1")

maybe_update::(a->b)->Maybe a->Maybe b
maybe_update _ Nothing=error "maybe_update: error 1"
maybe_update update (Just value)=Just (update value)

just_update::(a->b)->a->Maybe b
just_update update value=Just (update value)

map_insert::Ord a=>a->b->DM.Map a b->DM.Map a b
map_insert key value this_map=let (maybe_value,new_map)=DM.insertLookupWithKey (\_ _ this_value->this_value) key value this_map in case maybe_value of
    Nothing->new_map
    Just _->error "map_insert: error 1"

map_delete::Ord a=>a->DM.Map a b->DM.Map a b
map_delete key this_map=let (maybe_value,new_map)=DM.updateLookupWithKey (\_ _->Nothing) key this_map in case maybe_value of
    Nothing->error "map_delete: error 1"
    Just _->new_map

intmap_insert::Int->a->DIM.IntMap a->DIM.IntMap a
intmap_insert key value intmap=let (maybe_value,new_intmap)=DIM.insertLookupWithKey (\_ _ this_value->this_value) key value intmap in case maybe_value of
    Nothing->new_intmap
    Just _->error "intmap_insert: error 1"

intmap_delete::Int->DIM.IntMap a->DIM.IntMap a
intmap_delete key intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ _->Nothing) key intmap in case maybe_value of
    Nothing->error "intmap_delete: error 1"
    Just _->new_intmap

intset_insert::Int->DIS.IntSet->DIS.IntSet
intset_insert key intset=if DIS.member key intset then error "intset_insert: error 1" else DIS.insert key intset

intset_delete::Int->DIS.IntSet->DIS.IntSet
intset_delete key intset=if DIS.member key intset then DIS.delete key intset else error "intset_delete: error 1"

intset_foldm::Monad b=>(Int->a->b a)->DIS.IntSet->a->b a
intset_foldm transform=DIS.foldr (\key next value->transform key value>>=next) return