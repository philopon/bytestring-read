{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Data.ByteString.Read.Integral
    ( integral'
    , integral
    , int
    ) where

import Data.Proxy.Compat
import GHC.TypeLits.Compat

import Data.ByteString.Read.Class as C

-- $setup
-- >>> :set -XDataKinds -XOverloadedStrings
-- >>> import qualified Data.ByteString as S
-- >>> import qualified Data.ByteString.Lazy as L

integral_ :: (Radix b, Num n, Source s) => proxy b -> s -> (n, Int, s)
integral_ pn = loop 0 0
  where
    loop !i !d !s
        | C.null s                    = (i, d, s)
        | not (isDigit pn (C.head s)) = (i, d, s)
        | otherwise                   = loop
            (i * fromIntegral (natVal pn) + (fromIntegral $ unsafeToDigit pn (C.head s)))
            (d+1) (C.tail s)
{-# INLINABLE integral_ #-}

-- | convert bytestring into unsigned integral using radix
--
-- >>> integral' (Proxy :: Proxy 10) "12345" :: Maybe (Int, S.ByteString)
-- Just (12345,"")
-- >>> integral' (Proxy :: Proxy 2) "10112" :: Maybe (Int, L.ByteString)
-- Just (11,"2")
-- >>> integral' (Proxy :: Proxy 36) "Z" :: Maybe (Double, S.ByteString)
-- Just (35.0,"")
integral' :: (Radix b, Num n, Source s) => proxy b -> s -> Maybe (n, s)
integral' pn s0 = case integral_ pn s0 of
    (_, 0, _) -> Nothing
    (n, _, s) -> Just (n, s)
{-# INLINABLE integral' #-}
-- | @
-- integral = 'integral'' (Proxy :: Proxy 10)
-- @
integral :: (Num n, Source s) => s -> Maybe (n, s)
integral = integral' (Proxy :: Proxy 10)
{-# INLINABLE integral #-}

-- | @
-- int = integral
-- @
int :: Source s => s -> Maybe (Int, s)
int = integral
{-# INLINABLE int #-}
