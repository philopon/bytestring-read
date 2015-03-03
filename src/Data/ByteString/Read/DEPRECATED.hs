{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.ByteString.Read.DEPRECATED where

import Data.ByteString(ByteString)
import Data.ByteString.Read.Class
import Data.ByteString.Read.Fractional

{-# DEPRECATED EffectiveDigit "use ReadFractional" #-}
type EffectiveDigit = ReadFractional

{-# DEPRECATED floating "use fractional" #-}
floating :: EffectiveDigit r => ByteString -> Maybe (r, ByteString)
floating = fractional

{-# DEPRECATED Base "use Radix" #-}
type Base = Radix

{-# DEPRECATED floating10 "use fractional10" #-}
floating10 :: forall r. EffectiveDigit r => ByteString -> Maybe (r, ByteString) 
floating10 = fractional10

{-# DEPRECATED floating' "use floating'" #-}
floating' :: (Base b, EffectiveDigit r) => proxy b -> ByteString -> Maybe (r, ByteString) 
floating' = fractional'
