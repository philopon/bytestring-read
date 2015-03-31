module Data.ByteString.Read
    ( Source
      -- * fractional
    , ReadFractional
    , fractional
    , double

      -- * integral
    , integral
    , int

      -- * common
    , signed

      -- * DEPRECATED
    , module Data.ByteString.Read.DEPRECATED
    ) where

import Control.Arrow

import Data.ByteString.Read.Class as C
import Data.ByteString.Read.Integral
import Data.ByteString.Read.Fractional
import Data.ByteString.Read.DEPRECATED

-- $setup
-- >>> :set -XDataKinds -XOverloadedStrings
-- >>> import qualified Data.ByteString as S
-- >>> import qualified Data.ByteString.Lazy as L

-- | convert unsigned parser to signed parser.
--
-- this function can parse
--
-- * sign (+, -) (optional)
--
-- >>> signed double ("12.4" :: S.ByteString)
-- Just (12.4,"")
-- >>> signed double ("-3.21e3" :: L.ByteString)
-- Just (-3210.0,"")
-- >>> signed double ("+0x1f.4" :: S.ByteString)
-- Just (31.25,"")
signed :: (Source s, Num r) => (s -> Maybe (r, s)) -> s -> Maybe (r, s)
signed f s
    | C.null s = Nothing
    | C.head s == minus = first negate `fmap` f (C.tail s)
    | C.head s == plus  = f (C.tail s)
    | otherwise = f s
  where
    minus = 45
    plus  = 43
