{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Asterius.Types
  ( JSVal(..)
  , JSArrayBuffer(..)
  , JSString(..)
  , JSArray(..)
  , fromJSArrayBuffer
  , toJSArrayBuffer
  , fromJSString
  , toJSString
  , fromJSArray
  , toJSArray
  , jsStringDecodeUTF8
  , jsStringEncodeUTF8
  , jsStringDecodeLatin1
  , jsStringEncodeLatin1
  , jsStringDecodeUTF16LE
  , jsStringEncodeUTF16LE
  , jsStringDecodeUTF32LE
  , jsStringEncodeUTF32LE
  , js_freezeTempJSRef
  ) where

import Asterius.Magic
import GHC.Magic
import GHC.Prim
import GHC.Tuple
import GHC.Types

newtype JSVal =
  JSVal Int

newtype JSArrayBuffer =
  JSArrayBuffer JSVal

newtype JSString =
  JSString JSVal

newtype JSArray =
  JSArray JSVal

{-# INLINE fromJSArrayBuffer #-}
fromJSArrayBuffer :: JSArrayBuffer -> MutableByteArray# RealWorld
fromJSArrayBuffer buf = accursedUnutterableAddrToAny (c_fromJSArrayBuffer buf)

{-# INLINE toJSArrayBuffer #-}
toJSArrayBuffer :: Addr# -> Int -> JSArrayBuffer
toJSArrayBuffer = c_toJSArrayBuffer

{-# INLINE fromJSString #-}
fromJSString :: JSString -> [Char]
fromJSString s = accursedUnutterableAddrToAny (c_fromJSString s)

{-# INLINE toJSString #-}
toJSString :: [Char] -> JSString
toJSString s =
  runRW#
    (\s0 ->
       case unIO js_newString s0 of
         (# s1, i #) ->
           let w [] sx = (# sx, () #)
               w (c:cs) sx =
                 case unIO (js_appendString i c) sx of
                   (# sy, _ #) -> w cs sy
            in case w s s1 of
                 (# s2, _ #) ->
                   case unIO (js_freezeTempJSRef i) s2 of
                     (# _, r #) -> JSString r)

{-# INLINE fromJSArray #-}
fromJSArray :: JSArray -> [JSVal]
fromJSArray arr = accursedUnutterableAddrToAny (c_fromJSArray arr)

{-# INLINE toJSArray #-}
toJSArray :: [JSVal] -> JSArray
toJSArray arr =
  runRW#
    (\s0 ->
       case unIO js_newArray s0 of
         (# s1, i #) ->
           let w [] sx = (# sx, () #)
               w (v:vs) sx =
                 case unIO (js_appendArray i v) sx of
                   (# sy, _ #) -> w vs sy
            in case w arr s1 of
                 (# s2, _ #) ->
                   case unIO (js_freezeTempJSRef i) s2 of
                     (# _, r #) -> JSArray r)

foreign import ccall unsafe "__asterius_fromJSArrayBuffer" c_fromJSArrayBuffer
  :: JSArrayBuffer -> Addr#

foreign import ccall unsafe "__asterius_toJSArrayBuffer" c_toJSArrayBuffer
  :: Addr# -> Int -> JSArrayBuffer

foreign import ccall unsafe "__asterius_fromJSString" c_fromJSString
  :: JSString -> Addr#

foreign import ccall unsafe "__asterius_fromJSArray" c_fromJSArray
  :: JSArray -> Addr#

foreign import javascript "__asterius_jsffi.newTempJSRef('')" js_newString
  :: IO Int

foreign import javascript "__asterius_jsffi.mutTempJSRef(${1}, s => s + String.fromCodePoint(${2}))" js_appendString
  :: Int -> Char -> IO ()

foreign import javascript "__asterius_jsffi.newTempJSRef([])" js_newArray
  :: IO Int

foreign import javascript "__asterius_jsffi.mutTempJSRef(${1}, arr => (arr.push(${2}), arr))" js_appendArray
  :: Int -> JSVal -> IO ()

foreign import javascript "__asterius_jsffi.freezeTempJSRef(${1})" js_freezeTempJSRef
  :: Int -> IO JSVal

foreign import javascript "__asterius_jsffi.decodeUTF8(${1})" jsStringDecodeUTF8
  :: JSArrayBuffer -> JSString

foreign import javascript "__asterius_jsffi.encodeUTF8(${1})" jsStringEncodeUTF8
  :: JSString -> JSArrayBuffer

foreign import javascript "__asterius_jsffi.decodeLatin1(${1})" jsStringDecodeLatin1
  :: JSArrayBuffer -> JSString

foreign import javascript "__asterius_jsffi.encodeLatin1(${1})" jsStringEncodeLatin1
  :: JSString -> JSArrayBuffer

foreign import javascript "__asterius_jsffi.decodeUTF16LE(${1})" jsStringDecodeUTF16LE
  :: JSArrayBuffer -> JSString

foreign import javascript "__asterius_jsffi.encodeUTF16LE(${1})" jsStringEncodeUTF16LE
  :: JSString -> JSArrayBuffer

foreign import javascript "__asterius_jsffi.decodeUTF32LE(${1})" jsStringDecodeUTF32LE
  :: JSArrayBuffer -> JSString

foreign import javascript "__asterius_jsffi.encodeUTF32LE(${1})" jsStringEncodeUTF32LE
  :: JSString -> JSArrayBuffer
