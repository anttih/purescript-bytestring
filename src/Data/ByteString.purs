module Data.ByteString
  ( ByteString
  , CodeUnit(..)
  , CodePoint(..)
  , unconsCodeUnit
  , unconsCodePoint
  , fromString
  , length
  , slice
  , lengthCodePoints
  , codePointAt
  , toCodePointList
  , toCodePointArray
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.List (List)
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

instance Eq ByteString where
  eq x y = runFn2 eqImpl x y

instance Semigroup ByteString where
  append x y = runFn2 concatImpl x y

instance Monoid ByteString where
  mempty = fromString ""

lengthCodePoints :: ByteString -> Int
lengthCodePoints s = go 0 s
  where
  go count tail' = case unconsCodePoint tail' of
    Nothing -> count
    Just { tail } -> go (count + 1) tail

foreign import fromString :: String -> ByteString

foreign import length :: ByteString -> Int

foreign import showByteString :: ByteString -> String

foreign import eqImpl :: Fn2 ByteString ByteString Boolean

foreign import sliceImpl :: Fn3 Int Int ByteString ByteString

foreign import concatImpl :: Fn2 ByteString ByteString ByteString

foreign import toCodePointList :: ByteString -> List CodePoint

foreign import toCodePointArray :: ByteString -> Array CodePoint

slice :: Int -> Int -> ByteString -> ByteString
slice start end bs = runFn3 sliceImpl start end bs

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
