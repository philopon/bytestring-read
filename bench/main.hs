{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Read as R
import Criterion.Main
import qualified Data.ByteString.Lex.Double as L
import Data.ByteString.Char8(ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

test :: ByteString
test = "234239523214.3424346343524"

main :: IO ()
main = defaultMain
    [ bench "read"   $ nf (R.signed R.double) test
    , bench "text"   $ nf (T.signed T.double . T.decodeUtf8) test
    , bench "lexing" $ nf L.readDouble test
    ]
