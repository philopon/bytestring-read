{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Data.ByteString.Read.Integral
    ( integral'
    , integral
    , int
    ) where

import Data.ByteString(ByteString)
import qualified Data.ByteString as S
import Data.ByteString.Unsafe

import Data.Proxy.Compat
import GHC.TypeLits.Compat

import Data.ByteString.Read.Class

-- $setup
-- >>> :set -XDataKinds -XOverloadedStrings

integral_ :: (Radix b, Num n) => proxy b -> ByteString -> (n, Int, ByteString)
integral_ pn = loop 0 0
  where
    loop !i !d !s
        | S.null s                        = (i, d, s)
        | not (isDigit pn (unsafeHead s)) = (i, d, s)
        | otherwise                       = loop
            (i * fromIntegral (natVal pn) + (fromIntegral $ unsafeToDigit pn (unsafeHead s)))
            (d+1) (unsafeTail s)
{-# INLINABLE integral_ #-}

-- | convert bytestring into unsigned integral using radix
--
-- >>> integral' (Proxy :: Proxy 10) "12345" :: Maybe (Int, ByteString)
-- Just (12345,"")
-- >>> integral' (Proxy :: Proxy 2) "10112" :: Maybe (Int, ByteString)
-- Just (11,"2")
-- >>> integral' (Proxy :: Proxy 36) "Z" :: Maybe (Double, ByteString)
-- Just (35.0,"")
integral' :: (Radix b, Num n) => proxy b -> ByteString -> Maybe (n, ByteString)
integral' pn s0 = case integral_ pn s0 of
    (_, 0, _) -> Nothing
    (n, _, s) -> Just (n, s)
{-# INLINABLE integral' #-}
-- | @
-- integral = 'integral'' (Proxy :: Proxy 10)
-- @
integral :: Num n => ByteString -> Maybe (n, ByteString)
integral = integral' (Proxy :: Proxy 10)
{-# INLINABLE integral #-}

-- | @
-- int = integral
-- @
int :: ByteString -> Maybe (Int, ByteString)
int = integral
{-# INLINABLE int #-}
