{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Tests for `Quine.GL.Buffer`
--------------------------------------------------------------------------------
module Main where

import Test.Hspec
import Control.Exception.Base
import Control.Monad hiding (sequence)
import Control.Applicative
import Codec.Picture.Types
import Codec.Picture
import Data.Bits
import Data.Proxy
import qualified Data.ByteString.Lazy.Char8 as BS

import GHC.Generics
import Data.Default
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Internal (updPtr)
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc

import Quine.GL.Object
import Quine.GL.Error
import Quine.GL.Types
import Quine.GL.Texture
import Quine.GL
import Quine.StateVar
import Quine.SDL
import Quine.Cubemap
import Quine.MipmapChain
import Quine.Image
import Graphics.UI.SDL as SDL hiding (Texture)
import Linear

import Graphics.GL.Internal.Shared
import Graphics.GL.Types

--------------------------------------------------------------------------------
-- * Fixtures
--------------------------------------------------------------------------------

testImage :: Image Pixel8
testImage = Image 1 1 (V.fromList [0])

testDynamicImage :: DynamicImage
testDynamicImage = ImageY8 testImage

--------------------------------------------------------------------------------
-- * Setup
--------------------------------------------------------------------------------

withGLContext :: IO a -> IO a
withGLContext action = do
  bracket
    (do
      SDL.init SDL_INIT_EVERYTHING >>= err
      contextMajorVersion $= 4
      contextMinorVersion $= 1
      contextProfileMask $= SDL_GL_CONTEXT_PROFILE_CORE
      win <- withCString "shaders" $ \windowName -> createWindow windowName SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED 1 1 (SDL_WINDOW_OPENGL .|. SDL_WINDOW_HIDDEN)
      cxt <- glCreateContext win
      makeCurrent win  cxt
      return (win, cxt)
    )
   (\(win, cxt) -> glDeleteContext cxt >> destroyWindow win >> quit)
   (const action)

--------------------------------------------------------------------------------
-- * Tests
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ around_ withGLContext $ do
  -- * Textures generation
  describe "Texture generation" $ do
    it "at least some textures generatable" $ do
      texs <- gens 1024
      length (texs :: [Texture]) `shouldBe` 1024
      throwErrors
  
    it "a never bound Texture isn't a Texture" $ do
      tex <- gen :: IO Texture
      isa tex `shouldReturn` False
      throwErrors

    it "a once bound Texture is a Texture" $ do
      tex <- gen :: IO Texture
      boundTexture GL_TEXTURE_2D GL_TEXTURE_BINDING_2D $= tex
      boundTexture GL_TEXTURE_2D GL_TEXTURE_BINDING_2D $= def
      isa tex `shouldReturn` True
      throwErrors

    context "Cubemap textures" $ do
      it "with Image faces are storable" $ do
        let cubemap :: Cubemap (Image Pixel8)
            cubemap = pure testImage
        
        tex <- gen :: IO Texture
        boundTexture GL_TEXTURE_CUBE_MAP GL_TEXTURE_BINDING_CUBE_MAP $= tex
        store cubemap GL_TEXTURE_CUBE_MAP
        throwErrors
        upload cubemap GL_TEXTURE_CUBE_MAP 0
      it "with DynamicImage faces are storable" $ do
        let dynCubemap :: Cubemap (DynamicImage)
            dynCubemap = pure testDynamicImage
        
        tex <- gen :: IO Texture
        boundTexture GL_TEXTURE_CUBE_MAP GL_TEXTURE_BINDING_CUBE_MAP $= tex
        store dynCubemap GL_TEXTURE_CUBE_MAP
        throwErrors
        upload dynCubemap GL_TEXTURE_CUBE_MAP 0
        throwErrors

    context "MipMap textures" $ do
      it "with Image faces are storable" $ do
        let mips :: MipmapChain (Image Pixel8)
            mips = mkMipmapChain (generateImage (const.const 0) 4 4) [generateImage (const.const 0) 2 2, generateImage (const.const 0) 1 1]

        tex <- gen :: IO Texture
        boundTexture GL_TEXTURE_2D GL_TEXTURE_BINDING_2D $= tex
        store mips GL_TEXTURE_2D
        throwErrors
        upload mips GL_TEXTURE_2D 0
      it "with DynamicImage faces are storable" $ do
        let dynMips :: MipmapChain DynamicImage
            dynMips = mkMipmapChain (ImageY8 $ generateImage (const.const 0) 4 4) [ImageY8 $ generateImage (const.const 0) 2 2, ImageY8 $ generateImage (const.const 0) 1 1]
        
        tex <- gen :: IO Texture
        boundTexture GL_TEXTURE_2D GL_TEXTURE_BINDING_2D $= tex
        store dynMips GL_TEXTURE_2D
        throwErrors
        upload dynMips GL_TEXTURE_2D 0
        throwErrors
      
      -- it "with DynamicImage faces are storable" $ do
      --   tex <- gen :: IO Texture
      --   boundTexture GL_TEXTURE_CUBE_MAP GL_TEXTURE_BINDING_CUBE_MAP $= tex
      --   store testDynamicImage GL_TEXTURE_CUBE_MAP
      --   throwErrors

