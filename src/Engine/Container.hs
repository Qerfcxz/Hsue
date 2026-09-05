{-# LANGUAGE TupleSections #-}

module Engine.Container where

import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Control.Monad.Trans.State.Strict as CMTS
import qualified Data.Hashable as DH
import qualified Data.HashMap.Strict as DHMS
import qualified Data.HashSet as DHS
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Tuple as DT

int_map_lookup::ET.Has_call_stack=>Int->DIM.IntMap a->a
int_map_lookup key int_map=case DIM.lookup key int_map of
    Just value->value
    _->EF.empty_error

int_map_insert::ET.Has_call_stack=>Bool->Int->a->DIM.IntMap a->DIM.IntMap a
int_map_insert strict_conflict key value int_map=if strict_conflict then int_map_insert_strict key value int_map else DIM.insertWith (\_ this_value->this_value) key value int_map

int_map_insert_strict::ET.Has_call_stack=>Int->a->DIM.IntMap a->DIM.IntMap a
int_map_insert_strict key value int_map=let (maybe_value,new_int_map)=DIM.insertLookupWithKey (\_ _ this_value->this_value) key value int_map in case maybe_value of
    Nothing->new_int_map
    _->EF.empty_error

int_map_insert_maybe_lookup::ET.Has_call_stack=>Int->a->DIM.IntMap a->(DIM.IntMap a,Maybe a)
int_map_insert_maybe_lookup key value int_map=DT.swap (DIM.insertLookupWithKey (\_ _ this_value->this_value) key value int_map)

int_map_delete::ET.Has_call_stack=>Bool->Int->DIM.IntMap a->DIM.IntMap a
int_map_delete strict_exist key int_map=if strict_exist
    then let (maybe_value,new_int_map)=DIM.updateLookupWithKey (\_ _->Nothing) key int_map in case maybe_value of
        Nothing->EF.empty_error
        _->new_int_map
    else DIM.delete key int_map

int_map_delete_lookup::ET.Has_call_stack=>Int->DIM.IntMap a->(DIM.IntMap a,a)
int_map_delete_lookup key int_map=let (maybe_value,new_int_map)=DIM.updateLookupWithKey (\_ _->Nothing) key int_map in case maybe_value of
    Just value->(new_int_map,value)
    _->EF.empty_error

int_map_delete_maybe_lookup::ET.Has_call_stack=>Int->DIM.IntMap a->(DIM.IntMap a,Maybe a)
int_map_delete_maybe_lookup key int_map=DT.swap (DIM.updateLookupWithKey (\_ _->Nothing) key int_map)

int_map_update::ET.Has_call_stack=>Bool->Int->(a->a)->DIM.IntMap a->DIM.IntMap a
int_map_update strict_exist key update int_map=if strict_exist
    then let (maybe_value,new_int_map)=DIM.updateLookupWithKey (\_ value->Just (update value)) key int_map in case maybe_value of
        Nothing->EF.empty_error
        _->new_int_map
    else DIM.adjust update key int_map

int_map_update_lookup::ET.Has_call_stack=>Int->(a->a)->DIM.IntMap a->(DIM.IntMap a,a)
int_map_update_lookup key update int_map=let (maybe_value,new_int_map)=DIM.updateLookupWithKey (\_ value->Just (update value)) key int_map in case maybe_value of
    Just value->(new_int_map,value)
    _->EF.empty_error

int_map_update_maybe_lookup::ET.Has_call_stack=>Int->(a->a)->DIM.IntMap a->(DIM.IntMap a,Maybe a)
int_map_update_maybe_lookup key update int_map=let (maybe_value,new_int_map)=DIM.updateLookupWithKey (\_ value->Just (update value)) key int_map in (new_int_map,maybe_value)

int_map_functor_update::ET.Has_call_stack=>Functor b=>Int->(a->b a)->DIM.IntMap a->b (DIM.IntMap a)
int_map_functor_update key update=DIM.alterF (int_map_functor_update_a update) key

int_map_functor_update_a::ET.Has_call_stack=>Functor b=>(a->b a)->Maybe a->b (Maybe a)
int_map_functor_update_a update maybe_value=case maybe_value of
    Just value->fmap Just (update value)
    _->EF.empty_error

int_map_applicative_update::ET.Has_call_stack=>Applicative b=>Bool->Int->(a->b a)->DIM.IntMap a->b (DIM.IntMap a)
int_map_applicative_update strict_exist key update int_map=DIM.alterF (int_map_applicative_update_a strict_exist update) key int_map

int_map_applicative_update_a::ET.Has_call_stack=>Applicative b=>Bool->(a->b a)->Maybe a->b (Maybe a)
int_map_applicative_update_a strict_exist update maybe_value=case maybe_value of
    Just value->fmap Just (update value)
    Nothing->if strict_exist then EF.empty_error else pure Nothing

int_map_monad_mapping_update::ET.Has_call_stack=>Monad c=>Bool->(a->b->c b)->DIM.IntMap a->DIM.IntMap b->c (DIM.IntMap b)
int_map_monad_mapping_update strict_exist update first_int_map second_int_map=DIM.foldlWithKey' (\action key value->int_map_monad_mapping_update_a strict_exist key update value action) (return second_int_map) first_int_map

int_map_monad_mapping_update_a::ET.Has_call_stack=>Monad c=>Bool->Int->(a->b->c b)->a->c (DIM.IntMap b)->c (DIM.IntMap b)
int_map_monad_mapping_update_a strict_exist key update value action=do
    int_map<-action
    if strict_exist then int_map_functor_update key (update value) int_map else case DIM.lookup key int_map of
        Nothing->return int_map
        Just another_value->do
            new_another_value<-update value another_value
            return (DIM.insert key new_another_value int_map)

int_map_monad_fold::ET.Has_call_stack=>Monad c=>(Int->a->b->c b)->DIM.IntMap a->b->c b
int_map_monad_fold transform int_map value=DIM.foldlWithKey' (\action key first_value->action>>=transform key first_value) (return value) int_map

int_map_monad_action::ET.Has_call_stack=>Monad c=>(Int->a->b->c (b,d))->DIM.IntMap a->b->c (b,DIM.IntMap d)
int_map_monad_action action int_map value=do
    (new_int_map,new_value)<-CMTS.runStateT (DIM.traverseWithKey (\index first_value->CMTS.StateT {CMTS.runStateT=int_map_monad_action_a action index first_value}) int_map) value
    return (new_value,new_int_map)

int_map_monad_action_a::ET.Has_call_stack=>Monad c=>(Int->a->b->c (b,d))->Int->a->b->c (d,b)
int_map_monad_action_a action index first_value second_value=do
    (new_second_value,another_value)<-action index first_value second_value
    return (another_value,new_second_value)

int_set_insert::ET.Has_call_stack=>Int->DIS.IntSet->DIS.IntSet
int_set_insert key int_set=if DIS.member key int_set then EF.empty_error else DIS.insert key int_set

int_set_delete::ET.Has_call_stack=>Bool->Int->DIS.IntSet->DIS.IntSet
int_set_delete strict_exist key int_set=if strict_exist then if DIS.member key int_set then DIS.delete key int_set else EF.empty_error else DIS.delete key int_set

int_set_monad_fold::ET.Has_call_stack=>Monad b=>(Int->a->b a)->DIS.IntSet->a->b a
int_set_monad_fold transform int_set value=DIS.foldl' (\action key->action>>=transform key) (return value) int_set

hash_map_lookup::ET.Has_call_stack=>Eq a=>DH.Hashable a=>a->DHMS.HashMap a b->b
hash_map_lookup key hash_map=case DHMS.lookup key hash_map of
    Just value->value
    _->EF.empty_error

hash_map_insert::ET.Has_call_stack=>Eq a=>DH.Hashable a=>Bool->a->b->DHMS.HashMap a b->DHMS.HashMap a b
hash_map_insert strict_conflict key value hash_map=if strict_conflict then hash_map_insert_strict key value hash_map else DHMS.insertWith (\_ this_value->this_value) key value hash_map

hash_map_insert_strict::ET.Has_call_stack=>Eq a=>DH.Hashable a=>a->b->DHMS.HashMap a b->DHMS.HashMap a b
hash_map_insert_strict key value hash_map=case DHMS.lookup key hash_map of
    Nothing->DHMS.insert key value hash_map
    Just _->EF.empty_error

hash_map_delete::ET.Has_call_stack=>Eq a=>DH.Hashable a=>Bool->a->DHMS.HashMap a b->DHMS.HashMap a b
hash_map_delete strict_exist key hash_map=if strict_exist
    then case DHMS.lookup key hash_map of
        Nothing->EF.empty_error
        Just _->DHMS.delete key hash_map
    else DHMS.delete key hash_map

hash_set_insert::ET.Has_call_stack=>Eq a=>DH.Hashable a=>Bool->a->DHS.HashSet a->DHS.HashSet a
hash_set_insert strict_conflict key hash_set=if strict_conflict then if DHS.member key hash_set then EF.empty_error else DHS.insert key hash_set else DHS.insert key hash_set

hash_set_delete::ET.Has_call_stack=>Eq a=>DH.Hashable a=>Bool->a->DHS.HashSet a->DHS.HashSet a
hash_set_delete strict_exist key hash_set=if strict_exist then if DHS.member key hash_set then DHS.delete key hash_set else EF.empty_error else DHS.delete key hash_set

{-# INLINE int_map_lookup #-}
{-# INLINE int_map_insert #-}
{-# INLINE int_map_insert_strict #-}
{-# INLINE int_map_insert_maybe_lookup #-}
{-# INLINE int_map_delete #-}
{-# INLINE int_map_delete_lookup #-}
{-# INLINE int_map_delete_maybe_lookup #-}
{-# INLINE int_map_update #-}
{-# INLINE int_map_update_lookup #-}
{-# INLINE int_map_update_maybe_lookup #-}
{-# INLINE int_map_functor_update #-}
{-# INLINE int_map_functor_update_a #-}
{-# INLINE int_map_applicative_update #-}
{-# INLINE int_map_applicative_update_a #-}
{-# INLINE int_map_monad_mapping_update #-}
{-# INLINE int_map_monad_mapping_update_a #-}
{-# INLINE int_map_monad_fold #-}
{-# INLINE int_map_monad_action #-}
{-# INLINE int_map_monad_action_a #-}
{-# INLINE int_set_insert #-}
{-# INLINE int_set_delete #-}
{-# INLINE int_set_monad_fold #-}
{-# INLINE hash_map_lookup #-}
{-# INLINE hash_map_insert #-}
{-# INLINE hash_map_insert_strict #-}
{-# INLINE hash_map_delete #-}
{-# INLINE hash_set_insert #-}
{-# INLINE hash_set_delete #-}