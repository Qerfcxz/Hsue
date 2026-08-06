{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Underlying where

import Engine.Type
import qualified Error.Error as EE
import qualified Control.Monad as CM
import qualified Data.ByteString as DBS
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

catch_false::IO FCT.CBool->IO ()
catch_false io=do
    value<-io
    CM.unless (FMU.toBool value) (EE.quick_error "catch_false" 0)

catch_zero::(Eq a,Num a)=>a->IO ()
catch_zero number=case number of
    0->EE.quick_error "catch_zero" 0
    _->return ()

catch_null::FP.Ptr a->IO ()
catch_null ptr=CM.when (ptr==FP.nullPtr) (EE.quick_error "catch_null" 0)

return_catch_null::IO (FP.Ptr a)->IO (FP.Ptr a)
return_catch_null io=do
    ptr<-io
    if ptr==FP.nullPtr then EE.quick_error "return_catch_null" 0 else return ptr

with_string::String->(FP.Ptr FCT.CChar->IO a)->IO a
with_string string=DBS.useAsCString (DTE.encodeUtf8 (DT.pack string))

seq_poke_array::FS.Storable a=>Int->DS.Seq a->FP.Ptr a->IO ()
seq_poke_array size value ptr=CM.void (DF.foldlM (flip (seq_poke_array_a size)) ptr value)

seq_poke_array_a::FS.Storable a=>Int->a->FP.Ptr a->IO (FP.Ptr a)
seq_poke_array_a size value ptr=do
    FS.poke ptr value
    return (FP.plusPtr ptr size)

triple_reverse::(a,b,c)->(c,b,a)
triple_reverse (a,b,c)=(c,b,a)

to_extended::FCT.CFloat->Extended
to_extended number=Finite {number=number}

from_extended::Extended->FCT.CFloat
from_extended extended=case extended of
    Negative_infinity->0
    Finite {number}->number
    Positive_infinity->0

mebibyte::Num a=>a
mebibyte=1048576

nanosecond::Num a=>a
nanosecond=1000000000

millisecond::Num a=>a
millisecond=1000000