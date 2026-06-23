{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Helper where

import Engine.Container
import Engine.Type
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
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

widget_io_fold::Int->(a->Engine b->IO (Engine b,Widget b))->a->IO (Engine b,DIM.IntMap (Widget b))->IO (Engine b,DIM.IntMap (Widget b))
widget_io_fold key transform value accumulation=do
    (engine,intmap)<-accumulation
    (new_engine,new_value)<-transform value engine
    return (new_engine,intmap_insert key new_value intmap)

seq_poke_array::FS.Storable a=>Int->DS.Seq a->FP.Ptr a->IO ()
seq_poke_array size value ptr=CM.void (DF.foldlM (\this_ptr this_value->FS.poke this_ptr this_value>>return (FP.plusPtr this_ptr size)) ptr value)

point_addition::Point->Point->Point
point_addition first_point second_point=case first_point of
    Point {x=first_x,y=first_y}->case second_point of
        Point {x=second_x,y=second_y}->Point {x=first_x+second_x,y=first_y+second_y}

move_matrix::Point->Matrix->Matrix
move_matrix point matrix=case matrix of
    Matrix {x,y,x_x,x_y,y_x,y_y}->Matrix {x=point.x+x,y=point.y+y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y}

identity_matrix::Matrix
identity_matrix=Matrix {x=0,y=0,x_x=1,x_y=0,y_x=0,y_y=1}

to_extended::a->Extended a
to_extended number=Finite {number=number}

from_extended::Num a=>Extended a->a
from_extended extended=case extended of
    Negative_infinity->0
    Finite {number}->number
    Positive_infinity->0

mebibyte::Num a=>a
mebibyte=1048576

nanosecond::Num a=>a
nanosecond=1000000000