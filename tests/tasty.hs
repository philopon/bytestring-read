{-# LANGUAGE OverloadedStrings #-}
import Numeric
import Control.Monad
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Read
import Test.Tasty
import Test.Tasty.QuickCheck

(=~~) :: Double -> Double -> Bool
(=~~) a b = a == b || abs (a - b) <= max (abs a) (abs b) * 1e20

-- Word

newtype Word8 = Word8 String
    deriving Show

instance Arbitrary Word8 where
    arbitrary = do
        i <- choose (1, 50)
        n <- replicateM i $ choose ('0', '7')
        return $ Word8 $ "0o" ++ n

newtype Word10 = Word10 String
    deriving Show

instance Arbitrary Word10 where
    arbitrary = do
        i <- choose (1, 50)
        n <- replicateM i $ choose ('0', '9')
        return $ Word10 n

newtype Word16 = Word16 String
    deriving Show

instance Arbitrary Word16 where
    arbitrary = do
        i <- choose (1, 50)
        n <- replicateM i $ oneof [choose ('0', '9'), choose ('a', 'f'), choose ('A', 'F')]
        return $ Word16 $ "0x" ++ n

-- Int
newtype Int8 = Int8 String
    deriving Show

instance Arbitrary Int8 where
    arbitrary = do
        sign <- oneof $ map return ["", "-"]
        Word8 w <- arbitrary
        return . Int8 $ sign ++ w

newtype Int10 = Int10 String
    deriving Show

instance Arbitrary Int10 where
    arbitrary = do
        sign <- oneof $ map return ["", "-"]
        Word10 w <- arbitrary
        return . Int10 $ sign ++ w

newtype Int16 = Int16 String
    deriving Show

instance Arbitrary Int16 where
    arbitrary = do
        sign <- oneof $ map return ["", "-"]
        Word16 w <- arbitrary
        return . Int16 $ sign ++ w

-- Float
newtype Float10 = Float10 String
    deriving Show

instance Arbitrary Float10 where
    arbitrary = do
        Int10 q <- arbitrary
        Word10 r <- arbitrary
        return . Float10 $ q ++ '.': r

newtype SmallFloat10 = SmallFloat10 String
    deriving Show

instance Arbitrary SmallFloat10 where
    arbitrary = do
        sign <- oneof $ map return ["", "-"]
        i <- choose (0, 100)
        Word10 r <- arbitrary
        return . SmallFloat10 $ sign ++ "0." ++ replicate i '0' ++ r

newtype Float10Exp = Float10Exp String
    deriving Show

instance Arbitrary Float10Exp where
    arbitrary = do
        Float10 f <- arbitrary
        c <- oneof $ map return "eE"
        s <- oneof $ map return ["", "+", "-"]
        e <- choose (0, 10000 :: Int)
        return . Float10Exp $ f ++ c: s ++ show e 

main :: IO ()
main = defaultMain $ testGroup "read . show == id"
    [ testGroup "Integral"
        [ testProperty "Int" $ \i ->
            let Just (i', "") = signed integral . S.pack $ show i
            in (i :: Int) == i'
        ]
    , testGroup "Fractional"
        [ testProperty "showEFloat" $ \d ->
            let Just (d', "") = signed fractional . S.pack $ showEFloat Nothing d ""
            in (d :: Double) =~~ d'

        , testProperty "showFFloat" $ \d ->
            let Just (d', "") = signed fractional . S.pack $ showFFloat Nothing  d ""
            in (d :: Double) =~~ d'

        , testProperty "showGFloat" $ \d ->
            let Just (d', "") = signed fractional . S.pack $ showGFloat Nothing  d ""
            in (d :: Double) =~~ d'

        , testProperty "showHex" $ \i ->
            let Just (i', "") = signed fractional . (S.append "0x") . S.pack $ showHex (abs i) ""
            in fromIntegral (abs i :: Int) =~~ i'

        , testProperty "showOct" $ \i ->
            let Just (i', "") = signed fractional . (S.append "0o") . S.pack $ showOct (abs i) ""
            in fromIntegral (abs i :: Int) =~~ i'

        , testProperty "Word8" $ \(Word8 d) ->
            let Just (d', "") = signed fractional (S.pack d)
            in d' =~~ (read d :: Double)

        , testProperty "Word10" $ \(Word10 d) ->
            let Just (d', "") = signed fractional (S.pack d)
            in d' =~~ (read d :: Double)

        , testProperty "Word16" $ \(Word16 d) ->
            let Just (d', "") = signed fractional (S.pack d)
            in d' =~~ (read d :: Double)

        , testProperty "Int8" $ \(Int8 d) ->
            let Just (d', "") = signed fractional (S.pack d)
            in d' =~~ (read d :: Double)

        , testProperty "Int10" $ \(Int10 d) ->
            let Just (d', "") = signed fractional (S.pack d)
            in d' =~~ (read d :: Double)

        , testProperty "Int16" $ \(Int16 d) ->
            let Just (d', "") = signed fractional (S.pack d)
            in d' =~~ (read d :: Double)

        , testProperty "Float10" $ \(Float10 d) ->
            let Just (d', "") = signed fractional (S.pack d)
            in d' =~~ (read d :: Double)

        , testProperty "SmallFloat10" $ \(SmallFloat10 d) ->
            let Just (d', "") = signed fractional (S.pack d)
            in d' =~~ (read d :: Double)

        , testProperty "Float10Exp" $ \(Float10Exp d) ->
            let Just (d', "") = signed fractional (S.pack d)
            in d' =~~ (read d :: Double)
        ]
     ]
