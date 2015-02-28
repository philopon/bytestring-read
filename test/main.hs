{-# LANGUAGE OverloadedStrings #-}
import Numeric
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Read
import Test.Tasty
import Test.Tasty.QuickCheck

(=~~) :: Double -> Double -> Bool
(=~~) 0 0 = True
(=~~) a 0 = a < 1e20
(=~~) 0 b = b < 1e20
(=~~) a b = abs (a / b) - 1 < 1e20

main :: IO ()
main = defaultMain $ testGroup "read . show == id"
    [ testProperty "showEFloat" $ \d ->
        let Just (d', "") = signed readFloating . S.pack $ showEFloat Nothing d ""
        in (d :: Double) =~~ d'

    , testProperty "showFFloat" $ \d ->
        let Just (d', "") = signed readFloating . S.pack $ showFFloat Nothing  d ""
        in (d :: Double) =~~ d'

    , testProperty "showGFloat" $ \d ->
        let Just (d', "") = signed readFloating . S.pack $ showGFloat Nothing  d ""
        in (d :: Double) =~~ d'

    , testProperty "showHex" $ \i ->
        let Just (i', "") = signed readFloating . (S.append "0x") . S.pack $ showHex (abs i) ""
        in fromIntegral (abs i :: Int) =~~ i'

    , testProperty "showOct" $ \i ->
        let Just (i', "") = signed readFloating . (S.append "0o") . S.pack $ showOct (abs i) ""
        in fromIntegral (abs i :: Int) =~~ i'
     ]
