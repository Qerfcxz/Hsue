module Engine.Container where

import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Tuple as DT

map_lookup::Ord a=>a->DM.Map a b->b
map_lookup key this_map=case DM.lookup key this_map of
    Just value->value
    _->error "map_lookup: error 1"

map_insert::Ord a=>a->b->DM.Map a b->DM.Map a b
map_insert key value this_map=let (maybe_value,new_map)=DM.insertLookupWithKey (\_ _ this_value->this_value) key value this_map in case maybe_value of
    Nothing->new_map
    _->error "map_insert: error 1"

map_delete::Ord a=>a->DM.Map a b->DM.Map a b
map_delete key this_map=let (maybe_value,new_map)=DM.updateLookupWithKey (\_ _->Nothing) key this_map in case maybe_value of
    Nothing->error "map_delete: error 1"
    _->new_map

intmap_lookup::Int->DIM.IntMap a->a
intmap_lookup key intmap=case DIM.lookup key intmap of
    Just value->value
    _->error "intmap_lookup: error 1"

intmap_insert::Int->a->DIM.IntMap a->DIM.IntMap a
intmap_insert key value intmap=let (maybe_value,new_intmap)=DIM.insertLookupWithKey (\_ _ this_value->this_value) key value intmap in case maybe_value of
    Nothing->new_intmap
    _->error "intmap_insert: error 1"

intmap_insert_maybe_lookup::Int->a->DIM.IntMap a->(DIM.IntMap a,Maybe a)
intmap_insert_maybe_lookup key value intmap=DT.swap (DIM.insertLookupWithKey (\_ _ this_value->this_value) key value intmap)

intmap_delete::Int->DIM.IntMap a->DIM.IntMap a
intmap_delete key intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ _->Nothing) key intmap in case maybe_value of
    Nothing->error "intmap_delete: error 1"
    _->new_intmap

intmap_delete_lookup::Int->DIM.IntMap a->(DIM.IntMap a,a)
intmap_delete_lookup key intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ _->Nothing) key intmap in case maybe_value of
    Just value->(new_intmap,value)
    _->error "intmap_delete_lookup: error 1"

intmap_delete_maybe_lookup::Int->DIM.IntMap a->(DIM.IntMap a,Maybe a)
intmap_delete_maybe_lookup key intmap=DT.swap (DIM.updateLookupWithKey (\_ _->Nothing) key intmap)

intmap_update::Int->(a->a)->DIM.IntMap a->DIM.IntMap a
intmap_update key update intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ value->Just (update value)) key intmap in case maybe_value of
    Nothing->error "intmap_update: error 1"
    _->new_intmap

intmap_update_lookup::Int->(a->a)->DIM.IntMap a->(DIM.IntMap a,a)
intmap_update_lookup key update intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ value->Just (update value)) key intmap in case maybe_value of
    Just value->(new_intmap,value)
    _->error "intmap_update_lookup: error 1"

intmap_functor_update::Functor b=>Int->(a->b a)->DIM.IntMap a->b (DIM.IntMap a)
intmap_functor_update key update=DIM.alterF (intmap_functor_update_a update) key

intmap_functor_update_a::Functor b=>(a->b a)->Maybe a->b (Maybe a)
intmap_functor_update_a update maybe_value=case maybe_value of
    Just value->fmap Just (update value)
    _->error "intmap_functor_update_a: error 1"

intmap_monad_accumulate::Monad b=>Int->(a->b (a,c))->b (a,DIM.IntMap c)->b (a,DIM.IntMap c)
intmap_monad_accumulate key transform accumulate=do
    (coproduct,intmap)<-accumulate
    (new_coproduct,value)<-transform coproduct
    return (new_coproduct,intmap_insert key value intmap)

intset_insert::Int->DIS.IntSet->DIS.IntSet
intset_insert key intset=if DIS.member key intset then error "intset_insert: error 1" else DIS.insert key intset

intset_delete::Int->DIS.IntSet->DIS.IntSet
intset_delete key intset=if DIS.member key intset then DIS.delete key intset else error "intset_delete: error 1"

intset_monad_fold::Monad b=>(Int->a->b a)->DIS.IntSet->a->b a
intset_monad_fold transform=DIS.foldr (\key update value->transform key value>>=update) return