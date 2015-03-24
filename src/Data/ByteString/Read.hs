module Data.ByteString.Read
    ( -- * fractional
      ReadFractional
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

import Data.ByteString(ByteString)
import Data.ByteString.Unsafe
import qualified Data.ByteString as S

import Data.ByteString.Read.Class
import Data.ByteString.Read.Integral
import Data.ByteString.Read.Fractional
import Data.ByteString.Read.DEPRECATED

-- $setup
-- >>> :set -XDataKinds -XOverloadedStrings

-- | convert unsigned parser to signed parser.
--
-- this function can parse
--
-- * sign (+, -) (optional)
--
-- >>> signed double "12.4"
-- Just (12.4,"")
-- >>> signed double "-3.21e3"
-- Just (-3210.0,"")
-- >>> signed double "+0x1f.4"
-- Just (31.25,"")
signed :: Num r => (ByteString -> Maybe (r, ByteString)) -> ByteString -> Maybe (r, ByteString)
signed f s
    | S.null s = Nothing
    | unsafeHead s == minus = first negate `fmap` f (unsafeTail s)
    | unsafeHead s == plus  = f (unsafeTail s)
    | otherwise = f s
  where
    minus = 45
    plus  = 43
