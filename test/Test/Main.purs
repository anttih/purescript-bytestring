module Test.Main where

import Prelude

import Effect (Effect)
import Test.Assert (assert, assertEqual)
import Data.Maybe (Maybe(..))
import Data.ByteString (ByteString, CodeUnit(..), CodePoint(..))
import Data.ByteString as BS

main :: Effect Unit
main = do
  assertUnconsCodeUnitFail (BS.fromString "")
  assertUnconsCodeUnit (BS.fromString "foo") (CodeUnit 102)
  assertUnconsCodeUnit (BS.fromString "a") (CodeUnit 97)

  assertUnconsCodePointFail (BS.fromString "")
  assertUnconsCodePoint (BS.fromString "foo") (CodePoint 102)
  assertUnconsCodePoint (BS.fromString "a") (CodePoint 97)
  assertUnconsCodePoint (BS.fromString "ä") (CodePoint 228)
  assertUnconsCodePoint (BS.fromString "☃︎") (CodePoint 9731)

  assertEqual { actual: BS.lengthCodePoints (BS.fromString "foo"), expected: 3 }
  assertEqual { actual: BS.length (BS.fromString "foo"), expected: 3 }

  assertEqual { actual: BS.lengthCodePoints (BS.fromString "ää"), expected: 2 }
  assertEqual { actual: BS.length (BS.fromString "ää"), expected: 4 }

  assertEqual { actual: BS.lengthCodePoints (BS.fromString "🅐𝚕𝗂c̤𝘦 ｗä̤𝒔 𝒷ɘg̤̈⒤𝔫ⓝ𝒊n𝕘"), expected: 24 }
  assertEqual { actual: BS.length (BS.fromString "🅐𝚕𝗂c̤𝘦 ｗä̤𝒔 𝒷ɘg̤̈⒤𝔫ⓝ𝒊n𝕘"), expected: 63 }

  assertEqual { actual: BS.codePointAt (-1) (BS.fromString "a"), expected: Nothing }
  assertEqual { actual: BS.codePointAt 0 (BS.fromString ""), expected: Nothing }
  assertEqual { actual: BS.codePointAt 10 (BS.fromString "ää"), expected: Nothing }
  assertEqual { actual: BS.codePointAt 1 (BS.fromString "ää"), expected: Just (CodePoint 228) }
  assertEqual { actual: BS.codePointAt 0 (BS.fromString "aä"), expected: Just (CodePoint 97) }

  let s = BS.fromString "🅐𝚕𝗂c̤𝘦 ｗä̤𝒔 𝒷ɘg̤̈⒤𝔫ⓝ𝒊n𝕘"

  assert $ BS.fromString "" == BS.fromString ""
  assert $ BS.fromString "a" == BS.fromString "a"
  assert $ BS.fromString "aa" /= BS.fromString "a"
  assert $ BS.fromString "ää" /= BS.fromString "ä"
  assert $ s == s

  assertEqual { actual: BS.slice 0 1 (BS.fromString "foo bar"), expected: BS.fromString "f" }
  assertEqual { actual: BS.slice 0 2 (BS.fromString "foo bar"), expected: BS.fromString "fo" }
  assertEqual { actual: BS.slice 0 3 (BS.fromString "foo bar"), expected: BS.fromString "foo" }
  assertEqual { actual: BS.slice 0 4 (BS.fromString "foo bar"), expected: BS.fromString "foo " }
  assertEqual { actual: BS.slice 4 6 (BS.fromString "foo bar"), expected: BS.fromString "ba" }

assertUnconsCodeUnit :: ByteString -> CodeUnit -> Effect Unit
assertUnconsCodeUnit bs c =
  case BS.unconsCodeUnit bs of
    Nothing -> assert false
    Just { head } -> assertEqual { actual: head, expected: c }

assertUnconsCodeUnitFail :: ByteString -> Effect Unit
assertUnconsCodeUnitFail bs =
  case BS.unconsCodeUnit bs of
    Nothing -> pure unit
    Just _ -> assert false

assertUnconsCodePoint :: ByteString -> CodePoint -> Effect Unit
assertUnconsCodePoint bs c =
  case BS.unconsCodePoint bs of
    Nothing -> assert false
    Just { head } -> assertEqual { actual: head, expected: c }

assertUnconsCodePointFail :: ByteString -> Effect Unit
assertUnconsCodePointFail bs =
  case BS.unconsCodePoint bs of
    Nothing -> pure unit
    Just _ -> assert false

