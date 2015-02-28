{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Data.ByteString.Read (signed, readFloating) where

import Control.Applicative
import Control.Arrow(first)
import qualified Data.ByteString as S
import Data.ByteString(ByteString)
import Data.ByteString.Unsafe
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import GHC.Base

isNum8 :: CChar -> Bool
isNum8 i = 47 < i && i <= 55
{-# INLINE isNum8 #-}

toNum8 :: CChar -> Int
toNum8 i = fromEnum i - 48
{-# INLINE toNum8 #-}

isNum10 :: CChar -> Bool
isNum10 i = 47 < i && i <= 57
{-# INLINE isNum10 #-}

toNum10 :: CChar -> Int
toNum10 i = fromEnum i - 48
{-# INLINE toNum10 #-}

isNum16 :: CChar -> Bool
isNum16 i = 47 < i && i <= 57 || 65 <= i && i <= 70 || 97 <= i && i <= 102
{-# INLINE isNum16 #-}

toNum16 :: CChar -> Int
toNum16 i
    | 47 < i  && i <= 57 = fromEnum i - 48
    | 65 <= i && i <= 70 = fromEnum i - 55
    | otherwise          = fromEnum i - 87
{-# INLINE toNum16 #-}

dot :: CChar
dot = 46
{-# INLINE dot #-}

isE :: CChar -> Bool
isE i = i == 101 || i == 69
{-# INLINE isE #-}

isO :: CChar -> Bool
isO i = i == 111 || i == 79
{-# INLINE isO #-}

isX :: CChar -> Bool
isX i = i == 120 || i == 88
{-# INLINE isX #-}

isZero :: CChar -> Bool
isZero = (== 48)
{-# INLINE isZero #-}

toDouble :: Fractional a => Int -> Int -> Int -> a
toDouble base e f = fromIntegral f / (fromIntegral base ^ e)
{-# INLINE toDouble #-}

toDoubleN :: Floating a => Int -> Int -> Int -> a
toDoubleN base e f
    | e >= 0    = toDouble base e f
    | otherwise = fromIntegral f / (fromIntegral base ** fromIntegral e)
{-# INLINE toDoubleN #-}

readInt' :: (CChar -> Bool) -> (CChar -> Int) -> Int -> Int -> CStringLen -> IO (Maybe (Int, Int, CStringLen))
readInt' _     _     _    _  (_,     0)    = return Nothing
readInt' isNum toNum base i0 (cstr0, len0) = peek cstr0 >>= \case
    c0 | isNum c0  -> loop i0 0 (plusPtr cstr0 1) (len0 - 1) c0
       | otherwise -> return Nothing
  where
    loop !i !e cstr 0   c
        | isNum c   = return $ Just (i * base + toNum c, e + 1, (nullPtr, 0))
        | otherwise = return $ Just (i, e, (plusPtr cstr (-1), 1))
    loop !i !e cstr len c
        | isNum c   = peek cstr >>= loop (i * base + toNum c) (e + 1) (plusPtr cstr 1) (len - 1)
        | otherwise = return $ Just (i, e, (plusPtr cstr (-1), len + 1))

readFractional' :: (CChar -> Bool) -> (CChar -> Int) -> Int -> CStringLen -> IO (Maybe (Int, Int, CStringLen))
readFractional' isNum toNum base cstrLen0 = readI 0 cstrLen0 >>= \case
    Nothing -> return Nothing
    Just (i, _, (_, 0)) -> return $ Just (0, i, (nullPtr, 0))
    Just (i, _, csl@(cstr, len)) -> peek cstr >>= \case
        c | c == dot -> readI i (plusPtr cstr 1, len - 1) >>= \case
            Nothing -> return $ Just (0, i, csl)
            Just (f, e, csl') -> return $ Just (e, f, csl')
          | otherwise -> return $ Just (0, i, csl)
  where
    readI = readInt' isNum toNum base

readFloating10 :: Floating n => CStringLen -> IO (Maybe (n, CStringLen))
readFloating10 cstrLn0 = readFractional' isNum10 toNum10 10 cstrLn0 >>= \case
    Nothing -> return Nothing
    Just (e, f, csl@(cstr, len))
        | len < 2   -> return $ Just (toDouble 10 e f, csl)
        | otherwise -> do
            h <- peek cstr
            if isE h
                then do
                    (e', csl') <- exponential e (plusPtr cstr 1, len - 1)
                    return $ Just (toDoubleN 10 e' f, csl')
                else return $ Just (toDouble 10 e f, csl)
{-# SPECIALIZE readFloating10 :: CStringLen -> IO (Maybe (Double, CStringLen)) #-}
{-# SPECIALIZE readFloating10 :: CStringLen -> IO (Maybe (Float,  CStringLen)) #-}

exponential :: Int -> CStringLen -> IO (Int, CStringLen)
exponential e (cstr0, len0) = peek cstr0 >>= \case
    c | isNum10 c -> expNum cstr0 len0 >>= \case
        Nothing        -> return (e, (cstr0, len0))
        Just (e', csl) -> return (e - e', csl)
      | c == 43 || c == 45 -> expNum (plusPtr cstr0 1) (len0 - 1) >>= \case
        Nothing        -> return (e, (cstr0, len0))
        Just (e', csl) -> return (if c == 43 then e - e' else e + e', csl)
      | otherwise -> return (e, (cstr0, len0))
  where
    expNum cstr len = readInt' isNum10 toNum10 10 0 (cstr, len) >>= \case
        Nothing           -> return Nothing
        Just (e', _, csl) -> return $ Just (e', csl)

readFloating' :: Floating n => CStringLen -> IO (Maybe (n, CStringLen))
readFloating' (cstr, len)
    | len >= 2 = peek cstr >>= \case
        z | isZero z -> peek (plusPtr cstr 1 :: Ptr CChar) >>= \case
            i | isO i -> fmap (\(e,f,c) -> (toDouble  8 e f, c)) <$> readFractional' isNum8  toNum8   8 (plusPtr cstr 2, len - 2)
              | isX i -> fmap (\(e,f,c) -> (toDouble 16 e f, c)) <$> readFractional' isNum16 toNum16 16 (plusPtr cstr 2, len - 2)
              | otherwise -> readFloating10 (cstr, len)
          | otherwise -> readFloating10 (cstr, len)
    | otherwise = readFloating10 (cstr, len)
{-# SPECIALIZE readFloating' :: CStringLen -> IO (Maybe (Double, CStringLen)) #-}
{-# SPECIALIZE readFloating' :: CStringLen -> IO (Maybe (Float, CStringLen)) #-}

readFloating :: Floating n => ByteString -> Maybe (n, ByteString)
readFloating s = inlinePerformIO $ fmap conv <$> unsafeUseAsCStringLen s readFloating'
  where
    conv (n, (_,len)) = (n, unsafeDrop (S.length s - len) s)
{-# SPECIALIZE readFloating :: ByteString -> Maybe (Double, ByteString) #-}
{-# SPECIALIZE readFloating :: ByteString -> Maybe (Float, ByteString) #-}

signed :: Num a => (ByteString -> Maybe (a, ByteString)) -> ByteString -> Maybe (a, ByteString)
signed f = \s -> case S.uncons s of
    Nothing -> Nothing
    Just (43, s') -> f s'
    Just (45, s') -> first negate <$> f s'
    _            -> f s
{-# SPECIALIZE signed :: (ByteString -> Maybe (Double, ByteString)) -> ByteString -> Maybe (Double, ByteString) #-}
{-# SPECIALIZE signed :: (ByteString -> Maybe (Float, ByteString)) -> ByteString -> Maybe (Float, ByteString) #-}

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
