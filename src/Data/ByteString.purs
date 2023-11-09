module Data.ByteString
  ( ByteString
  , CodeUnit(..)
  , CodePoint(..)
  , unconsCodeUnit
  , unconsCodePoint
  , fromString
  , size
  , length
  , codePointAt
  ) where

import Prelude

import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..))

newtype CodePoint = CodePoint Int

derive newtype instance eqCodePoint :: Eq CodePoint
derive newtype instance showCodePoint :: Show CodePoint

-- A UTF-8 code unit. This would be our new `Char`.
newtype CodeUnit = CodeUnit Int

derive newtype instance eqCodeUnit :: Eq CodeUnit
derive newtype instance showCodeUnit :: Show CodeUnit

foreign import data ByteString :: Type

instance Show ByteString where
  show = showByteString

-- type Result2 = { head :: CodePoint, tail :: ByteString }

-- size is actually constant time, so this is here just for
-- the benchmarks
size :: ByteString -> Int
size s = go 0 s
  where
  go count tail' = case unconsCodeUnit tail' of
    Nothing -> count
    Just { tail } -> go (count + 1) tail

length :: ByteString -> Int
length s = go 0 s
  where
  go count tail' = case unconsCodePoint tail' of
    Nothing -> count
    Just { tail } -> go (count + 1) tail

foreign import fromString :: String -> ByteString

foreign import showByteString :: ByteString -> String

foreign import unconsCodeUnitImpl
  :: Fn3
       ByteString
       (forall a. a -> Maybe a)
       (Maybe { head :: CodeUnit, tail :: ByteString })
       (Maybe { head :: CodeUnit, tail :: ByteString })

unconsCodeUnit :: ByteString -> Maybe { head :: CodeUnit, tail :: ByteString }
unconsCodeUnit bs = runFn3 unconsCodeUnitImpl bs Just Nothing

foreign import unconsCodePointImpl
  :: Fn3
       ByteString
       (forall a. a -> Maybe a)
       (Maybe { head :: CodePoint, tail :: ByteString })
       (Maybe { head :: CodePoint, tail :: ByteString })

unconsCodePoint :: ByteString -> Maybe { head :: CodePoint, tail :: ByteString }
unconsCodePoint bs = runFn3 unconsCodePointImpl bs Just Nothing

codePointAt :: Int -> ByteString -> Maybe CodePoint
codePointAt n bs = runFn4 codePointAtImpl Just Nothing n bs

foreign import codePointAtImpl
  :: Fn4
       (forall a. a -> Maybe a)
       (Maybe CodePoint)
       Int
       ByteString
       (Maybe CodePoint)
