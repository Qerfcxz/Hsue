{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}

module Error.Type where

#ifdef HAS_CALL_STACK

import Data.Kind
import GHC.Stack

type Has_call_stack=HasCallStack::Constraint

#else

import Data.Kind

type Has_call_stack=()::Constraint

#endif