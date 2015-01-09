{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A chain of textures with decreasing sizes (currently not enforced)
--------------------------------------------------------------------
module Quine.MipmapChain
  ( -- * Chained Mipmap Images
    MipmapChain
  -- ** Construction
  , mkMipmapChain
  , mipMapChain
  -- ** Query
  , mipMapBase
  , maxMipMapLevel
  -- ** Predicates
  , hasMipMaps
  ) where

import Prelude hiding (zipWith,tail,head,length,sequence_)
import Codec.Picture
import Codec.Picture.Types (dynamicMap)
import Data.Foldable (sequence_)
import Data.Proxy
import Data.List.NonEmpty
import Graphics.GL.Ext.ARB.TextureStorage
import Quine.Image

-- | The first element in the 'MipmapChain' is the base element
-- (e.g. 'Texture' or 'FilePath'). Every 'MipmapChain' has a base
-- element (aka is never empty see: 'NonEmpty'). The other elements in the chain are the mipmap
-- levels in resoloution descending order.
type MipmapChain tex = NonEmpty tex

-- | Creates a mipmap chain from a base and a mip map list
mkMipmapChain :: tex -> [tex] -> MipmapChain tex
mkMipmapChain base mipmaps = base :| mipmaps
{-# INLINE mkMipmapChain #-}

-- | creates a 'MipmapChain' from a list. At least one element as the base element
-- is required. On an empty list 'Nothing' is returned.
mipMapChain :: [tex] -> Maybe (MipmapChain tex)
mipMapChain = nonEmpty
{-# INLINE mipMapChain #-}

-- | Extracts the first element in the mipmap chain
mipMapBase :: MipmapChain tex -> tex
mipMapBase = head
{-# INLINE mipMapBase #-}

-- | Predicate if a `MipmapChain` has destinct mipmap levels beside the base (0th)
hasMipMaps :: MipmapChain a -> Bool
hasMipMaps = not . null . tail
{-# INLINE hasMipMaps #-}

-- | The number of mipmap levels (without the base)
maxMipMapLevel :: MipmapChain tex -> Int
maxMipMapLevel mips = length mips - 1
{-# INLINE maxMipMapLevel #-}

instance (ImageFormat a, Image2D (Image a)) => Image2D (MipmapChain (Image a)) where
  upload chain t _l = sequence_ $ zipWith (\img l -> upload img t l) chain (mkMipmapChain 0 [1..])
  store chain t = do
    let base@(Image w h _) = mipMapBase chain
    glTexStorage2D t (fromIntegral $ maxMipMapLevel chain + 1) (internalFormat base) (fromIntegral w) (fromIntegral h)

instance Image2D (MipmapChain DynamicImage) where
  upload chain t _l = sequence_ $ zipWith (\img l -> upload img t l) chain (mkMipmapChain 0 [1..])
  store chain t = 
    let base = (mipMapBase chain) in glTexStorage2D t (fromIntegral $ maxMipMapLevel chain + 1) fmt (fromIntegral $ dynamicMap imageWidth base) (fromIntegral $ dynamicMap imageHeight base)  
    where 
    fmt = case (mipMapBase chain) of
      (ImageY8 i)      -> internalFormat i
      (ImageY16 i)     -> internalFormat i
      (ImageYF i)      -> internalFormat i
      (ImageYA8 i)     -> internalFormat i
      (ImageYA16 i)    -> internalFormat i
      (ImageRGB8 i)    -> internalFormat i
      (ImageRGB16 i)   -> internalFormat i
      (ImageRGBF i)    -> internalFormat i
      (ImageRGBA8 i)   -> internalFormat i
      (ImageRGBA16 i)  -> internalFormat i
      -- the following formats are converted by the Image2D DynamicImage instance
      (ImageYCbCr8 _)  -> internalFormat (Proxy :: Proxy PixelRGB8)
      (ImageCMYK8 _)   -> internalFormat (Proxy :: Proxy PixelRGB8)
      (ImageCMYK16 _)  -> internalFormat (Proxy :: Proxy PixelRGB16)
