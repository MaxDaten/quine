{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE PatternSynonyms    #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Quine.GL.Texture
  ( Texture
  , TextureWrapping
  , TextureMinificationFilter
  , TextureMagnificationFilter
  , MipmapLevel
  , TextureLayer
  , TextureUnit
  -- * Texture Binding
  , boundTexture
  -- * Texture Targets
  , TextureTarget, TextureBinding
  -- * Texture Parameter
  , TextureParameter
  -- ** Bound Based
  -- $texParameter
  , texParameterf
  , texParameteri
  , texParameterfv
  , texParameter2f
  , texParameter3f
  , texParameter4f
  , texParameteriv
  , texParameter2i
  , texParameter3i
  , texParameter4i
  , texParameterIiv
  , texParameterIuiv
  -- * Texture Unit
  , activeTexture
  -- * Debug
  , showTextureTarget
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data
import Data.Default
import Data.Int
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear
import Linear.V
import Quine.GL.Object
import Quine.StateVar

type TextureTarget = GLenum
type TextureBinding = GLenum
type TextureParameter = GLenum
type TextureWrapping = GLenum
type TextureMinificationFilter = GLenum
type TextureMagnificationFilter = GLenum
type MipmapLevel = GLsizei
type TextureLayer = GLint
type TextureUnit = GLuint

newtype Texture = Texture GLuint deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Object Texture where
  object = coerce
  isa i = (GL_FALSE /=) `liftM` glIsTexture (coerce i)
  deletes xs = liftIO $ allocaArray n $ \p -> do
    pokeArray p (coerce xs)
    glDeleteTextures (fromIntegral n) p
    where n = length xs

instance Gen Texture where
  gens n = liftIO $ allocaArray n $ \p -> do
    glGenTextures (fromIntegral n) p
    map Texture <$> peekArray n p

instance Default Texture where
  def = Texture 0

boundTexture :: TextureTarget -> TextureBinding -> StateVar Texture
boundTexture target binding = StateVar g s where
  g = do
    i <- alloca $ liftM2 (>>) (glGetIntegerv binding) peek
    return $ Texture (fromIntegral i)
  s = glBindTexture target . coerce

-- * Texture Parameter

-- $texParameter
-- Settings for the current bound 'Texture'.
-- Consider using 'Quine.GL.Sampler' to store settings 'Texture' independent

texParameterf :: TextureTarget -> TextureParameter -> StateVar Float
texParameterf t p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameterfv t p . castPtr <*> peek
  s = glTexParameterf t p

texParameteri :: TextureTarget -> TextureParameter -> StateVar Int32
texParameteri t p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameteriv t p . castPtr <*> peek
  s = glTexParameteri t p

texParameterfv' :: Storable (f Float) => TextureTarget -> TextureParameter -> StateVar (f Float)
texParameterfv' t p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameterfv t p . castPtr <*> peek
  s v = alloca $ (>>) <$> (`poke` v) <*> glTexParameterfv t p . castPtr

texParameterfv :: Dim n => TextureTarget -> TextureParameter -> StateVar (V n Float)
texParameterfv = texParameterfv'

texParameter2f :: TextureTarget -> TextureParameter -> StateVar (V2 Float)
texParameter2f = texParameterfv'

texParameter3f :: TextureTarget -> TextureParameter -> StateVar (V3 Float)
texParameter3f = texParameterfv'

texParameter4f :: TextureTarget -> TextureParameter -> StateVar (V4 Float)
texParameter4f = texParameterfv'

texParameteriv' :: Storable (f Int32) => TextureTarget -> TextureParameter -> StateVar (f Int32)
texParameteriv' t p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameteriv t p . castPtr <*> peek
  s v = alloca $ (>>) <$> (`poke` v) <*> glTexParameteriv t p . castPtr

texParameteriv :: Dim n => TextureTarget -> TextureParameter -> StateVar (V n Int32)
texParameteriv = texParameteriv'

texParameter2i :: TextureTarget -> TextureParameter -> StateVar (V2 Int32)
texParameter2i = texParameteriv'

texParameter3i :: TextureTarget -> TextureParameter -> StateVar (V3 Int32)
texParameter3i = texParameteriv'

texParameter4i :: TextureTarget -> TextureParameter -> StateVar (V4 Int32)
texParameter4i = texParameteriv'

texParameterIiv :: Dim n => TextureTarget -> TextureParameter -> StateVar (V n Int32)
texParameterIiv t p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameterIiv t p . castPtr <*> peek
  s v = alloca $ (>>) <$> (`poke` v) <*> glTexParameterIiv t p . castPtr

texParameterIuiv :: Dim n => TextureTarget -> TextureParameter -> StateVar (V n Word32)
texParameterIuiv t p = StateVar g s where
  g = alloca $ (>>) <$> glGetTexParameterIuiv t p . castPtr <*> peek
  s v = alloca $ (>>) <$> (`poke` v) <*> glTexParameterIuiv t p . castPtr

-- * Texture Unit

activeTexture :: StateVar Word32
activeTexture = StateVar g s where
  g = fmap fromIntegral $ alloca $ liftM2 (>>) (glGetIntegerv GL_ACTIVE_TEXTURE) peek
  s n = glActiveTexture (GL_TEXTURE0 + n)

-- * Debug

showTextureTarget :: TextureTarget -> String
showTextureTarget = \case
  GL_TEXTURE_1D -> "GL_TEXTURE_1D"
  GL_TEXTURE_1D_ARRAY -> "GL_TEXTURE_1D_ARRAY"
  GL_TEXTURE_2D -> "GL_TEXTURE_2D"
  GL_TEXTURE_2D_ARRAY -> "GL_TEXTURE_2D_ARRAY"
  GL_TEXTURE_2D_MULTISAMPLE -> "GL_TEXTURE_2D_MULTISAMPLE" 
  GL_TEXTURE_2D_MULTISAMPLE_ARRAY -> "GL_TEXTURE_2D_MULTISAMPLE_ARRAY"
  GL_TEXTURE_3D -> "GL_TEXTURE_3D"
  GL_TEXTURE_BUFFER -> "GL_TEXTURE_BUFFER"
  GL_TEXTURE_CUBE_MAP -> "GL_TEXTURE_CUBE_MAP"
  GL_TEXTURE_CUBE_MAP_ARRAY -> "GL_TEXTURE_CUBE_MAP_ARRAY"
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X -> "GL_TEXTURE_CUBE_MAP_NEGATIVE_X"
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y -> "GL_TEXTURE_CUBE_MAP_NEGATIVE_Y"
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z -> "GL_TEXTURE_CUBE_MAP_NEGATIVE_Z"
  GL_TEXTURE_CUBE_MAP_POSITIVE_X -> "GL_TEXTURE_CUBE_MAP_POSITIVE_X"
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y -> "GL_TEXTURE_CUBE_MAP_POSITIVE_Y"
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z -> "GL_TEXTURE_CUBE_MAP_POSITIVE_Z"
  GL_TEXTURE_RECTANGLE -> "GL_TEXTURE_RECTANGLE"
  _ -> "Unknown TextureTarget"
