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

map_insert::Ord a=>a->b->DM.Map a b->DM.Map a b
map_insert key value this_map=let (maybe_value,new_map)=DM.insertLookupWithKey (\_ _ this_value->this_value) key value this_map in case maybe_value of
    Nothing->new_map
    Just _->error "map_insert: error 1"

map_delete::Ord a=>a->DM.Map a b->DM.Map a b
map_delete key this_map=let (maybe_value,new_map)=DM.updateLookupWithKey (\_ _->Nothing) key this_map in case maybe_value of
    Nothing->error "map_delete: error 1"
    Just _->new_map

intmap_lookup::Int->DIM.IntMap a->a
intmap_lookup key intmap=case DIM.lookup key intmap of
    Nothing->error "intmap_lookup: error 1"
    Just value->value

intmap_insert::Int->a->DIM.IntMap a->DIM.IntMap a
intmap_insert key value intmap=let (maybe_value,new_intmap)=DIM.insertLookupWithKey (\_ _ this_value->this_value) key value intmap in case maybe_value of
    Nothing->new_intmap
    Just _->error "intmap_insert: error 1"

intmap_delete::Int->DIM.IntMap a->DIM.IntMap a
intmap_delete key intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ _->Nothing) key intmap in case maybe_value of
    Nothing->error "intmap_delete: error 1"
    Just _->new_intmap

intmap_delete_lookup::Int->DIM.IntMap a->(DIM.IntMap a,a)
intmap_delete_lookup key intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ _->Nothing) key intmap in case maybe_value of
    Nothing->error "intmap_delete_lookup: error 1"
    Just value->(new_intmap,value)

intmap_update::Int->(a->a)->DIM.IntMap a->DIM.IntMap a
intmap_update key update intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ value->Just (update value)) key intmap in case maybe_value of
    Nothing->error "intmap_update: error 1"
    Just _->new_intmap

intmap_update_lookup::Int->(a->a)->DIM.IntMap a->(DIM.IntMap a,a)
intmap_update_lookup key update intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ value->Just (update value)) key intmap in case maybe_value of
    Nothing->error "intmap_update_lookup: error 1"
    Just value->(new_intmap,value)

intset_insert::Int->DIS.IntSet->DIS.IntSet
intset_insert key intset=if DIS.member key intset then error "intset_insert: error 1" else DIS.insert key intset

intset_delete::Int->DIS.IntSet->DIS.IntSet
intset_delete key intset=if DIS.member key intset then DIS.delete key intset else error "intset_delete: error 1"

intset_foldm::Monad b=>(Int->a->b a)->DIS.IntSet->a->b a
intset_foldm transform=DIS.foldr (\key next value->transform key value>>=next) return