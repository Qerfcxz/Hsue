module Engine.Other where

import qualified Data.IntSet as DIS

maybe_update::(a->b)->Maybe a->Maybe b
maybe_update _ Nothing=error "maybe_update: error 1"
maybe_update update (Just value)=Just (update value)

intset_foldm::Monad b=>(Int->a->b a)->DIS.IntSet->a->b a
intset_foldm transform=DIS.foldr (\key next value->transform key value>>=next) return