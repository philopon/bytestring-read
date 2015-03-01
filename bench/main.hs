{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Read as R
import Criterion.Main
import qualified Data.ByteString.Lex.Double as L
import Data.ByteString.Char8(ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Scientific

testB :: ByteString
testB = "2342395232123424.3424346343524e3"

main :: IO ()
main = defaultMain
    [ bench "read"   $ nf (R.signed R.floating :: ByteString -> Maybe (Double, ByteString)) testB
    , bench "text"   $ nf (T.signed T.double . T.decodeUtf8) testB
    , bench "lexing" $ nf L.readDouble testB
    , bench "attoparsec" $ nf (\s -> A.parseOnly (fmap toRealFloat A.scientific :: A.Parser Double) s) testB
    ]
