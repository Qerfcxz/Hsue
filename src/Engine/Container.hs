{-# LANGUAGE TupleSections #-}

module Engine.Container where

import qualified Error.Error as EE
import qualified Data.Hashable as DH
import qualified Data.HashMap.Strict as DHMS
import qualified Data.HashSet as DHS
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Tuple as DT

int_map_lookup::Int->DIM.IntMap a->a
int_map_lookup key int_map=case DIM.lookup key int_map of
    Just value->value
    _->EE.quick_error "int_map_lookup" 0

int_map_insert::Int->a->DIM.IntMap a->DIM.IntMap a
int_map_insert key value int_map=let (maybe_value,new_int_map)=DIM.insertLookupWithKey (\_ _ this_value->this_value) key value int_map in case maybe_value of
    Nothing->new_int_map
    _->EE.quick_error "int_map_insert" 0

int_map_insert_maybe_lookup::Int->a->DIM.IntMap a->(DIM.IntMap a,Maybe a)
int_map_insert_maybe_lookup key value int_map=DT.swap (DIM.insertLookupWithKey (\_ _ this_value->this_value) key value int_map)

int_map_delete::Int->DIM.IntMap a->DIM.IntMap a
int_map_delete key int_map=let (maybe_value,new_int_map)=DIM.updateLookupWithKey (\_ _->Nothing) key int_map in case maybe_value of
    Nothing->EE.quick_error "int_map_delete" 0
    _->new_int_map

int_map_delete_lookup::Int->DIM.IntMap a->(DIM.IntMap a,a)
int_map_delete_lookup key int_map=let (maybe_value,new_int_map)=DIM.updateLookupWithKey (\_ _->Nothing) key int_map in case maybe_value of
    Just value->(new_int_map,value)
    _->EE.quick_error "int_map_delete_lookup" 0

int_map_delete_maybe_lookup::Int->DIM.IntMap a->(DIM.IntMap a,Maybe a)
int_map_delete_maybe_lookup key int_map=DT.swap (DIM.updateLookupWithKey (\_ _->Nothing) key int_map)

int_map_update::Int->(a->a)->DIM.IntMap a->DIM.IntMap a
int_map_update key update int_map=let (maybe_value,new_int_map)=DIM.updateLookupWithKey (\_ value->Just (update value)) key int_map in case maybe_value of
    Nothing->EE.quick_error "int_map_update" 0
    _->new_int_map

int_map_update_safe::Int->(a->a)->DIM.IntMap a->DIM.IntMap a
int_map_update_safe key update=DIM.update (Just . update) key

int_map_update_lookup::Int->(a->a)->DIM.IntMap a->(DIM.IntMap a,a)
int_map_update_lookup key update int_map=let (maybe_value,new_int_map)=DIM.updateLookupWithKey (\_ value->Just (update value)) key int_map in case maybe_value of
    Just value->(new_int_map,value)
    _->EE.quick_error "int_map_update_lookup" 0

int_map_functor_update::Functor b=>Int->(a->b a)->DIM.IntMap a->b (DIM.IntMap a)
int_map_functor_update key update=DIM.alterF (int_map_functor_update_a update) key

int_map_functor_update_a::Functor b=>(a->b a)->Maybe a->b (Maybe a)
int_map_functor_update_a update maybe_value=case maybe_value of
    Just value->fmap Just (update value)
    _->EE.quick_error "int_map_functor_update_a" 0

int_map_applicative_update_safe::Applicative c=>(a->b->c b)->DIM.IntMap a->DIM.IntMap b->c (DIM.IntMap b)
int_map_applicative_update_safe update int_map=DIM.traverseWithKey (\key value->int_map_applicative_update_a key update int_map value)

int_map_applicative_update::Applicative c=>(a->b->c b)->DIM.IntMap a->DIM.IntMap b->c (DIM.IntMap b)
int_map_applicative_update update first_int_map second_int_map=if DIS.isSubsetOf (DIM.keysSet first_int_map) (DIM.keysSet second_int_map) then DIM.traverseWithKey (\key value->int_map_applicative_update_a key update first_int_map value) second_int_map else EE.quick_error "int_map_applicative_update" 0

int_map_applicative_update_a::Applicative c=>Int->(a->b->c b)->DIM.IntMap a->b->c b
int_map_applicative_update_a key update int_map value=case DIM.lookup key int_map of
    Nothing->pure value
    Just new_value->update new_value value

int_map_monad_fold::Monad c=>(Int->a->b->c b)->DIM.IntMap a->b->c b
int_map_monad_fold transform=DIM.foldrWithKey (\key first_value update second_value->transform key first_value second_value>>=update) return

int_map_monad_action::Monad c=>(Int->a->b->c (b,d))->DIM.IntMap a->b->c (b,DIM.IntMap d)
int_map_monad_action action int_map value=DIM.foldlWithKey' (\this_action index this_value->this_action>>=int_map_monad_action_a index action this_value) (pure (value,DIM.empty)) int_map

int_map_monad_action_a::Monad c=>Int->(Int->a->b->c (b,d))->a->(b,DIM.IntMap d)->c (b,DIM.IntMap d)
int_map_monad_action_a index action first_value (second_value,int_map)=do
    (new_second_value,new_first_value)<-action index first_value second_value
    return (new_second_value,int_map_insert index new_first_value int_map)

int_set_insert::Int->DIS.IntSet->DIS.IntSet
int_set_insert key int_set=if DIS.member key int_set then EE.quick_error "int_set_insert" 0 else DIS.insert key int_set

int_set_delete::Int->DIS.IntSet->DIS.IntSet
int_set_delete key int_set=if DIS.member key int_set then DIS.delete key int_set else EE.quick_error "int_set_delete" 0

int_set_monad_fold::Monad b=>(Int->a->b a)->DIS.IntSet->a->b a
int_set_monad_fold transform=DIS.foldr (\key update value->transform key value>>=update) return

hash_map_lookup::(Eq a,DH.Hashable a)=>a->DHMS.HashMap a b->b
hash_map_lookup key hash_map=case DHMS.lookup key hash_map of
    Just value->value
    _->EE.quick_error "hash_map_lookup" 0

hash_map_insert::(Eq a,DH.Hashable a)=>a->b->DHMS.HashMap a b->DHMS.HashMap a b
hash_map_insert key value hash_map=case DHMS.alterF (,Just value) key hash_map of
    (Nothing,new_hash_map)->new_hash_map
    _->EE.quick_error "hash_map_insert" 0

hash_map_delete::(Eq a,DH.Hashable a)=>a->DHMS.HashMap a b->DHMS.HashMap a b
hash_map_delete key hash_map=case DHMS.alterF (,Nothing) key hash_map of
    (Just _,new_hash_map)->new_hash_map
    _->EE.quick_error "hash_map_delete" 0

hash_set_insert::(Eq a,DH.Hashable a)=>a->DHS.HashSet a->DHS.HashSet a
hash_set_insert key hash_set=if DHS.member key hash_set then EE.quick_error "hash_set_insert" 0 else DHS.insert key hash_set

hash_set_delete::(Eq a,DH.Hashable a)=>a->DHS.HashSet a->DHS.HashSet a
hash_set_delete key hash_set=if DHS.member key hash_set then DHS.delete key hash_set else EE.quick_error "hash_set_delete" 0

{-# INLINE int_map_lookup #-}
{-# INLINE int_map_insert #-}
{-# INLINE int_map_insert_maybe_lookup #-}
{-# INLINE int_map_delete #-}
{-# INLINE int_map_delete_lookup #-}
{-# INLINE int_map_delete_maybe_lookup #-}
{-# INLINE int_map_update #-}
{-# INLINE int_map_update_safe #-}
{-# INLINE int_map_update_lookup #-}
{-# INLINE int_map_functor_update #-}
{-# INLINE int_map_functor_update_a #-}
{-# INLINE int_map_applicative_update_safe #-}
{-# INLINE int_map_applicative_update #-}
{-# INLINE int_map_applicative_update_a #-}
{-# INLINE int_map_monad_fold #-}
{-# INLINE int_map_monad_action #-}
{-# INLINE int_map_monad_action_a #-}
{-# INLINE int_set_insert #-}
{-# INLINE int_set_delete #-}
{-# INLINE int_set_monad_fold #-}
{-# INLINE hash_map_lookup #-}
{-# INLINE hash_map_insert #-}
{-# INLINE hash_map_delete #-}
{-# INLINE hash_set_insert #-}
{-# INLINE hash_set_delete #-}