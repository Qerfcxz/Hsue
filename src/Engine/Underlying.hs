{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Underlying where

import Engine.Type
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Control.Monad as CM
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

catch_false::ET.Has_call_stack=>IO FCT.CBool->IO ()
catch_false io=do
    value<-io
    CM.unless (FMU.toBool value) EF.empty_error

catch_zero::ET.Has_call_stack=>Eq a=>Num a=>a->IO ()
catch_zero number=case number of
    0->EF.empty_error
    _->return ()

catch_null::ET.Has_call_stack=>FP.Ptr a->IO ()
catch_null ptr=CM.when (ptr==FP.nullPtr) EF.empty_error

return_catch_null::ET.Has_call_stack=>IO (FP.Ptr a)->IO (FP.Ptr a)
return_catch_null io=do
    ptr<-io
    if ptr==FP.nullPtr then EF.empty_error else return ptr

with_string::ET.Has_call_stack=>String->(FP.Ptr FCT.CChar->IO a)->IO a
with_string string=DBS.useAsCString (DTE.encodeUtf8 (DT.pack string))

seq_poke_array::ET.Has_call_stack=>FS.Storable a=>Int->DS.Seq a->FP.Ptr a->IO ()
seq_poke_array size value ptr=CM.void (DF.foldlM (flip (seq_poke_array_a size)) ptr value)

seq_poke_array_a::ET.Has_call_stack=>FS.Storable a=>Int->a->FP.Ptr a->IO (FP.Ptr a)
seq_poke_array_a size value ptr=do
    FS.poke ptr value
    return (FP.plusPtr ptr size)

triple_reverse::ET.Has_call_stack=>(a,b,c)->(c,b,a)
triple_reverse (a,b,c)=(c,b,a)

vector_io_map::ET.Has_call_stack=>Int->(Int->a->b->IO (b,c))->DV.Vector a->DVM.IOVector c->b->IO b
vector_io_map index action first_vector second_vector value=if index<0 then return value else do
    (new_value,new_new_value)<-action index (first_vector DV.! index) value
    DVM.unsafeWrite second_vector index new_new_value
    vector_io_map (index-1) action first_vector second_vector new_value

move_clip::ET.Has_call_stack=>Point->Clip->Clip
move_clip point clip=case clip of
    Clip {x,y,half_width,half_height,min_u,min_v,max_u,max_v}->Clip {x=x+point.x,y=y+point.y,half_width=half_width,half_height=half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v}

multiply_color::ET.Has_call_stack=>Color->Color->Color
multiply_color first_color second_color=case first_color of
    Color {red=first_red,green=first_green,blue=first_blue,alpha=first_alpha}->case second_color of
        Color {red=second_red,green=second_green,blue=second_blue,alpha=second_alpha}->Color {red=first_red*second_red,green=first_green*second_green,blue=first_blue*second_blue,alpha=first_alpha*second_alpha}

combine_arrange::ET.Has_call_stack=>Arrange->Arrange->Arrange
combine_arrange first_arrange second_arrange=case first_arrange of
    Arrange {point=first_point,matrix=first_matrix,color=first_color}->case second_arrange of
        Arrange {point=second_point,matrix=second_matrix,color=second_color}->case first_point of
            Point {x=first_point_x,y=first_point_y}->case second_point of
                Point {x=second_point_x,y=second_point_y}->case first_matrix of
                    Matrix {x=first_matrix_x,y=first_matrix_y,x_x=first_matrix_x_x,x_y=first_matrix_x_y,y_x=first_matrix_y_x,y_y=first_matrix_y_y}->case second_matrix of
                        Matrix {x=second_matrix_x,y=second_matrix_y,x_x=second_matrix_x_x,x_y=second_matrix_x_y,y_x=second_matrix_y_x,y_y=second_matrix_y_y}->Arrange {point=let new_x=second_point_x+second_matrix_x-first_point_x-first_matrix_x in let new_y=second_point_y+second_matrix_y-first_point_y-first_matrix_y in Point {x=first_point_x+first_matrix_x-second_matrix_x+first_matrix_x_x*new_x+first_matrix_x_y*new_y,y=first_point_y+first_matrix_y-second_matrix_y+first_matrix_y_x*new_x+first_matrix_y_y*new_y},matrix=Matrix {x=second_matrix_x,y=second_matrix_y,x_x=first_matrix_x_x*second_matrix_x_x+first_matrix_x_y*second_matrix_y_x,x_y=first_matrix_x_x*second_matrix_x_y+first_matrix_x_y*second_matrix_y_y,y_x=first_matrix_y_x*second_matrix_x_x+first_matrix_y_y*second_matrix_y_x,y_y=first_matrix_y_x*second_matrix_x_y+first_matrix_y_y*second_matrix_y_y},color=multiply_color first_color second_color}

to_extended::ET.Has_call_stack=>FCT.CFloat->Extended
to_extended number=Finite {number=number}

from_extended::ET.Has_call_stack=>Extended->FCT.CFloat
from_extended extended=case extended of
    Negative_infinity->0
    Finite {number}->number
    Positive_infinity->0

mebibyte::ET.Has_call_stack=>Num a=>a
mebibyte=1048576

nanosecond::ET.Has_call_stack=>Num a=>a
nanosecond=1000000000

millisecond::ET.Has_call_stack=>Num a=>a
millisecond=1000000

{-# INLINE catch_false #-}
{-# INLINE catch_zero #-}
{-# INLINE catch_null #-}
{-# INLINE return_catch_null #-}
{-# INLINE with_string #-}
{-# INLINE seq_poke_array #-}
{-# INLINE seq_poke_array_a #-}
{-# INLINE triple_reverse #-}
{-# INLINE move_clip #-}
{-# INLINE multiply_color #-}
{-# INLINE combine_arrange #-}
{-# INLINE to_extended #-}
{-# INLINE from_extended #-}
{-# INLINE mebibyte #-}
{-# INLINE nanosecond #-}
{-# INLINE millisecond #-}