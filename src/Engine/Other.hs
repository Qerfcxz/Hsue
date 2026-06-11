{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Other where

import Engine.Type
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Sequence as DS
import qualified Data.Tuple as DT
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

catch_false::IO FCT.CBool->IO ()
catch_false io=do
    value<-io
    CM.unless (FMU.toBool value) (error "catch_false: error 1")

catch_zero::(Eq a,Num a)=>a->IO ()
catch_zero number=case number of
    0->error "catch_zero: error 1"
    _->return ()

catch_null::FP.Ptr a->IO ()
catch_null ptr=CM.when (ptr==FP.nullPtr) (error "catch_null: error 1")

return_catch_null::IO (FP.Ptr a)->IO (FP.Ptr a)
return_catch_null io=do
    ptr<-io
    if ptr==FP.nullPtr then error "return_catch_null: error 1" else return ptr

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

intmap_delete::Int->DIM.IntMap a->DIM.IntMap a
intmap_delete key intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ _->Nothing) key intmap in case maybe_value of
    Nothing->error "intmap_delete: error 1"
    _->new_intmap

intmap_delete_lookup::Int->DIM.IntMap a->(DIM.IntMap a,a)
intmap_delete_lookup key intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ _->Nothing) key intmap in case maybe_value of
    Just value->(new_intmap,value)
    _->error "intmap_delete_lookup: error 1"

intmap_update::Int->(a->a)->DIM.IntMap a->DIM.IntMap a
intmap_update key update intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ value->Just (update value)) key intmap in case maybe_value of
    Nothing->error "intmap_update: error 1"
    _->new_intmap

intmap_update_lookup::Int->(a->a)->DIM.IntMap a->(DIM.IntMap a,a)
intmap_update_lookup key update intmap=let (maybe_value,new_intmap)=DIM.updateLookupWithKey (\_ value->Just (update value)) key intmap in case maybe_value of
    Just value->(new_intmap,value)
    _->error "intmap_update_lookup: error 1"

intmap_calculate::Int->(a->(a,b))->DIM.IntMap a->(DIM.IntMap a,b)
intmap_calculate key calculate intmap=DT.swap (DIM.alterF (intmap_calculate_a calculate) key intmap)

intmap_calculate_a::(a->(a,b))->Maybe a->(b,Maybe a)
intmap_calculate_a calculate maybe_value=case maybe_value of
    Just value->let (new_value,calculate_value)=calculate value in (calculate_value,Just new_value)
    _->error "intmap_calculate_a: error 1"

intset_insert::Int->DIS.IntSet->DIS.IntSet
intset_insert key intset=if DIS.member key intset then error "intset_insert: error 1" else DIS.insert key intset

intset_delete::Int->DIS.IntSet->DIS.IntSet
intset_delete key intset=if DIS.member key intset then DIS.delete key intset else error "intset_delete: error 1"

intset_foldm::Monad b=>(Int->a->b a)->DIS.IntSet->a->b a
intset_foldm transform=DIS.foldr (\key next value->transform key value>>=next) return

seq_poke_array::FS.Storable a=>Int->DS.Seq a->FP.Ptr a->IO ()
seq_poke_array size value ptr=CM.void (DF.foldlM (\this_ptr this_value->FS.poke this_ptr this_value>>return (FP.plusPtr this_ptr size)) ptr value)

apply_matrix::Matrix->Point->Point
apply_matrix matrix point=let x=point.x-matrix.x in let y=point.y-matrix.y in Point {x=matrix.x+matrix.x_x*x+matrix.x_y*y,y=matrix.y+matrix.y_x*x+matrix.y_y*y}

identity_matrix::FCT.CFloat->FCT.CFloat->Matrix
identity_matrix x y=Matrix {x=x,y=y,x_x=1,x_y=0,y_x=0,y_y=1}

mebibyte::Num a=>a
mebibyte=1048576

nanosecond::Num a=>a
nanosecond=1000000000