{-# OPTIONS_GHC -Wno-orphans #-}

module Asterius.Internals.FastString
  ( GHC.FastString
  , GHC.bytesFS
  , GHC.unpackFS
  , GHC.mkFastString
  , GHC.mkFastStringByteString
  , toShortByteString
  , fromShortByteString
  , GHC.nullFS
  ) where

import Data.Binary
import qualified Data.ByteString.Short as SBS
import Data.ByteString.Short (ShortByteString)
import qualified FastString as GHC

instance Binary GHC.FastString where
  {-# INLINE put #-}
  put = put . GHC.bytesFS
  {-# INLINE get #-}
  get = GHC.mkFastStringByteString <$> get

{-# INLINE toShortByteString #-}
toShortByteString :: GHC.FastString -> ShortByteString
toShortByteString = SBS.toShort . GHC.bytesFS

{-# INLINE fromShortByteString #-}
fromShortByteString :: ShortByteString -> GHC.FastString
fromShortByteString = GHC.mkFastStringByteString . SBS.fromShort
