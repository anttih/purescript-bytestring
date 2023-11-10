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

foreign import lengthNative :: String -> Int

main :: Effect Unit
main = do
  input <- readTextFile "./alice.txt"
  inputUnicode <- readTextFile "./alice.unicode.txt"

  inputBs <- pure (BS.fromString input)
  inputBsUnicode <- pure (BS.fromString inputUnicode)

  benchNativeStringLength input inputUnicode

  benchCodeUnits inputBs inputBsUnicode
  benchCodePoints inputBs inputBsUnicode

  where
  
  benchNativeStringLength :: String -> String -> Effect Unit
  benchNativeStringLength ascii unicode = do
    log "---"
    log "native string-length ascii" 
    benchWith 100 \_ -> lengthNative ascii

    log "---"
    log "native string-length unicode" 
    benchWith 100 \_ -> lengthNative unicode

  benchCodeUnits :: ByteString -> ByteString -> Effect Unit
  benchCodeUnits ascii unicode = do
    log "---"
    log "ByteString length (code units) ascii"
    benchWith 100 \_ -> BS.length ascii

    log "---"
    log "ByteString length (code units) unicode"
    benchWith 100 \_ -> BS.length unicode

  benchCodePoints :: ByteString -> ByteString -> Effect Unit
  benchCodePoints ascii unicode = do
    log "---"
    log "ByteString length (code points) ascii"
    benchWith 100 \_ -> BS.lengthCodePoints ascii

    log "---"
    log "ByteString length (code points) unicode"
    benchWith 100 \_ -> BS.lengthCodePoints unicode

