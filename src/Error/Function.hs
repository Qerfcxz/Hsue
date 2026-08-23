module Error.Function where

import Error.Type

empty_error::Has_call_stack=>a
empty_error=error ""

{-# INLINE empty_error #-}