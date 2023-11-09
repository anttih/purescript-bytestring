module Bench.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (benchWith)
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.ByteString (ByteString, CodePoint)
import Data.ByteString as BS

foreign import unconsCodePointImpl ::
  Fn3
  String
  (forall a. a -> Maybe a)
  (Maybe { head :: CodePoint, tail :: String })
  (Maybe { head :: CodePoint, tail :: String })

foreign import readTextFile :: String -> Effect String

main :: Effect Unit
main = do
  input <- readTextFile "./alice.txt"
  inputUnicode <- readTextFile "./alice.unicode.txt"

  inputBs <- pure (BS.fromString input)
  inputBsUnicode <- pure (BS.fromString inputUnicode)

  benchNativeString input
  benchNativeStringUnicode inputUnicode

  benchCodeUnits inputBs
  benchCodePoints inputBsUnicode

  where
  
  countNativeString :: String -> Int
  countNativeString s = go 0 s
    where

    go count tail' = case runFn3 unconsCodePointImpl tail' Just Nothing of
      Nothing -> count
      Just { tail } -> go (count + 1) tail

  size :: ByteString -> Int
  size s = go 0 s
    where
    go count tail' = case BS.unconsCodeUnit tail' of
      Nothing -> count
      Just { tail } -> go (count + 1) tail

  benchNativeString :: String -> Effect Unit
  benchNativeString input = do
    log "---"
    log "uncons native string codepoint with ascii input" 
    benchWith 100 \_ -> countNativeString input

  benchNativeStringUnicode :: String -> Effect Unit
  benchNativeStringUnicode input = do
    log "---"
    log "uncons native string codepoint with unicode input" 
    benchWith 100 \_ -> countNativeString input

  benchCodeUnits :: ByteString -> Effect Unit
  benchCodeUnits input = do
    log "---"
    log "uncons ByteString code unit"
    benchWith 100 \_ -> size input

  benchCodePoints :: ByteString -> Effect Unit
  benchCodePoints input = do
    log "---"
    log "uncons ByteString code point"
    benchWith 100 \_ -> BS.length input
