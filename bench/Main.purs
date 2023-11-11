module Bench.Main where

import Prelude

import Data.ByteString (CodePoint)
import Data.ByteString as BS
import Data.Function.Uncurried (Fn2, Fn3, runFn2)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (benchWith)

main :: Effect Unit
main = do
  input <- readTextFile "./alice.txt"
  inputUnicode <- readTextFile "./alice.unicode.txt"

  inputBs <- pure (BS.fromString input)
  inputBsUnicode <- pure (BS.fromString inputUnicode)

  log "native string-length ascii" 
  benchWith 100 \_ -> lengthNative input
  log "---"

  log "native string-length unicode" 
  benchWith 100 \_ -> lengthNative inputUnicode
  log "---"

  log "ByteString length (code units) ascii"
  benchWith 100 \_ -> BS.length inputBs
  log "---"

  log "ByteString length (code units) unicode"
  benchWith 100 \_ -> BS.length inputBsUnicode
  log "---"

  log "ByteString length (code points) ascii"
  benchWith 100 \_ -> BS.lengthCodePoints inputBs
  log "---"

  log "ByteString length (code points) unicode"
  benchWith 100 \_ -> BS.lengthCodePoints inputBsUnicode
  log "---"

  log "native string-append (concat)"
  benchWith 100 \_ -> runFn2 stringAppend inputUnicode inputUnicode
  log "---"
  
  log "ByteString concat"
  benchWith 100 \_ -> inputBsUnicode <> inputBsUnicode

foreign import unconsCodePointImpl ::
  Fn3
  String
  (forall a. a -> Maybe a)
  (Maybe { head :: CodePoint, tail :: String })
  (Maybe { head :: CodePoint, tail :: String })

foreign import readTextFile :: String -> Effect String

foreign import lengthNative :: String -> Int

foreign import stringAppend :: Fn2 String String String

