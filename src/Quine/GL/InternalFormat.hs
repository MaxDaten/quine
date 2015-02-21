{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PatternSynonyms     #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2015 Edward Kmett and Jan-Philip Loos
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Retrieve information about implementation-dependent support 
-- for internal formats
--------------------------------------------------------------------
module Quine.GL.InternalFormat
  ( -- * Plain Getting 
    getInternalFormatv
  , getInternalFormats
  , getInternalFormat1
  , checkInternalFormat
  -- * Convenient Getter
  -- $disclaimer
  , getFilterSupport
  ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Proxy
import Data.Maybe (fromJust)
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.TypeLits
import Graphics.GL.Core43
import Graphics.GL.Types
import Linear.V
import qualified Data.Vector as V
import Quine.GL.Texture (TextureTarget)
import Quine.GL.Pixel (InternalFormat)

-- * Plain getter

getInternalFormatv :: forall m (n :: Nat). (MonadIO m, Dim n) => TextureTarget -> InternalFormat -> GLenum -> m (V n Int)
getInternalFormatv target internalformat pname =
	liftM (fromJust . fromVector . V.fromList) $ getInternalFormats target internalformat pname (reflectDim (Proxy::Proxy n))

getInternalFormats :: MonadIO m => TextureTarget -> InternalFormat -> GLenum -> Int -> m [Int]
getInternalFormats target internalformat pname num =
 liftIO $ allocaArray num $ \p -> do
 	glGetInternalformativ target internalformat pname (fromIntegral num) (castPtr p)
 	peekArray num p

getInternalFormat1 :: MonadIO m => TextureTarget -> InternalFormat -> GLenum -> m Int
getInternalFormat1 target internalformat pname = liftM (V.head . toVector) get
 where
 get :: MonadIO m => m (V 1 Int) 
 get = getInternalFormatv target internalformat pname

-- | Conenvient to check format compatibilty
checkInternalFormat :: MonadIO m => TextureTarget -> InternalFormat -> GLenum -> m Bool
checkInternalFormat t i p = liftM (GL_TRUE ==) $ getInternalFormat1 t i p

--------------------------------------------------------------------------------
-- * Program Uniforms
--------------------------------------------------------------------------------

-- $disclaimer
-- some convenient preselected well defined getter 
-- (far from complete, feel free to add further )

-- | is set to either True or False to indicate support or lack thereof for filter
-- modes other than GL_NEAREST or GL_NEAREST_MIPMAP for the specified internal format.
getFilterSupport :: MonadIO m => TextureTarget -> InternalFormat -> m Bool
getFilterSupport t i = checkInternalFormat t i GL_MIPMAP

