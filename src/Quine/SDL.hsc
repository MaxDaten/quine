{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Prettier SDL bindings
--------------------------------------------------------------------
module Quine.SDL
  ( 
    init
  , initSubSystem
  , quit
  , quitSubSystem
  , wasInit
  -- * Versioning
  , version
  , revision
  , revisionNumber
  -- * Attribute StateVars
  , contextMajorVersion
  , contextMinorVersion
  , contextFlags
  , contextProfileMask
  , redSize
  , greenSize
  , blueSize
  , alphaSize
  , bufferSize
  , depthSize
  , stencilSize
  , accumRedSize
  , accumGreenSize
  , accumBlueSize
  , accumAlphaSize
  , multiSampleBuffers
  , multiSampleSamples
  , stereo
  , acceleratedVisual
  , doubleBuffer
  , shareWithCurrentContext
  , framebufferSRGBCapable
  , swapInterval
  , windowDisplayMode
  , desktopDisplayMode
  , windowSize
  , makeCurrent
  -- * Extensible Exceptions
  , SDLException(..)
  -- * Utilities
  , err
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.Typeable
import Data.Version as Data
import Foreign
import Foreign.C
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.StateVar
import qualified Graphics.UI.SDL as SDL
import Prelude hiding (init)
import Quine.GL

#include "SDL.h"

#if MIN_VERSION_sdl2(1,1,4)
type InitFlag = SDL.InitFlag
#else
type InitFlag = Word32
#endif

-- | This is thrown in the event of an error in the @Quine.SDL@ combinators
newtype SDLException = SDLException String
  deriving (Show, Typeable)

instance Exception SDLException

-- | Treat negative return codes as prompting an error check.
err :: CInt -> IO ()
err e 
  | e < 0 = do
    msg <- SDL.getError >>= peekCString
    SDL.clearError
    when (msg /= "") $ throw $ SDLException msg
  | otherwise = return ()

-- * Initialization

init :: MonadIO m => InitFlag -> m ()
init x = liftIO (SDL.init x >>= err)

initSubSystem :: MonadIO m => InitFlag -> m ()
initSubSystem x = liftIO (SDL.initSubSystem x >>= err)

quit :: MonadIO m => m ()
quit = liftIO SDL.quit

quitSubSystem :: MonadIO m => InitFlag -> m ()
quitSubSystem = liftIO . SDL.quitSubSystem

wasInit :: MonadIO m => InitFlag -> m InitFlag
wasInit = liftIO . SDL.wasInit

-- * Version

-- | Get the Version (and Revision)
version :: MonadIO m => m Data.Version
version = liftIO $ alloca $ \p -> do
  SDL.getVersion p
  SDL.Version x y z <- peek p
  w <- revisionNumber
  return $ Data.Version (fromIntegral <$> [fromIntegral x,fromIntegral y,fromIntegral z, w]) []

revision :: MonadIO m => m String
revision = liftIO $ SDL.getRevision >>= peekCString

revisionNumber :: MonadIO m => m Int
revisionNumber = liftIO $ fromIntegral <$> SDL.getRevisionNumber
-- * Attribute StateVars
  
-- | get\/set @SDL_GL_RED_SIZE@, the minimum number of bits for the red channel of the color buffer; defaults to 3
redSize :: StateVar Int
redSize = attr SDL.glAttrRedSize

-- | get\/set @SDL_GL_GREEN_SIZE@, the minimum number of bits for the green channel of the color buffer; defaults to 3
greenSize :: StateVar Int
greenSize = attr SDL.glAttrGreenSize

-- | get\/set @SDL_GL_BLUE_SIZE@, the minimum number of bits for the blue channel of the color buffer; defaults to 2
blueSize :: StateVar Int
blueSize = attr SDL.glAttrBlueSize

-- | get\/set @SDL_GL_ALPHA_SIZE@, the minimum number of bits for the alpha channel of the color buffer; defaults to 0
alphaSize :: StateVar Int
alphaSize = attr SDL.glAttrAlphaSize

-- | get\/set @SDL_GL_BUFFER_SIZE@, the minimum number of bits for frame buffer size; defaults to 0
bufferSize :: StateVar Int
bufferSize = attr SDL.glAttrBufferSize

-- | get\/set @SDL_GL_DEPTH_SIZE@, the minimum number of bits in the depth buffer; defaults to 16
depthSize :: StateVar Int
depthSize = attr SDL.glAttrDepthSize

-- | get\/set @SDL_GL_STENCIL_SIZE@, the minimum number of bits in the stencil buffer; defaults to 0
stencilSize :: StateVar Int
stencilSize = attr SDL.glAttrStencilSize

-- | get\/set @SDL_GL_ACCUM_RED_SIZE@, the minimum number of bits for the red channel of the accumulation buffer; defaults to 0
accumRedSize :: StateVar Int
accumRedSize = attr SDL.glAttrAccumRedSize

-- | get\/set @SDL_GL_ACCUM_GREEN_SIZE@, the minimum number of bits for the green channel of the accumulation buffer; defaults to 0
accumGreenSize :: StateVar Int
accumGreenSize = attr SDL.glAttrAccumGreenSize

-- | get\/set @SDL_GL_ACCUM_BLUE_SIZE@, the minimum number of bits for the blue channel of the accumulation buffer; defaults to 0
accumBlueSize :: StateVar Int
accumBlueSize = attr SDL.glAttrAccumBlueSize

-- | get\/set @SDL_GL_ACCUM_ALPHA_SIZE@, the minimum number of bits for the alpha channel of the accumulation buffer; defaults to 0
accumAlphaSize :: StateVar Int
accumAlphaSize = attr SDL.glAttrAccumAlphaSize

-- | get\/set @SDL_GL_MULTISAMPLEBUFFERS@, the number of buffers used for multisample anti-aliasing; defaults to 0; see <https://wiki.libsdl.org/SDL_GLattr#multisample Remarks> for details
multiSampleBuffers :: StateVar Int
multiSampleBuffers  = attr SDL.glAttrMultiSampleBuffers

-- | get\/set @SDL_GL_MULTISAMPLESAMPLES@, the number of samples used around the current pixel used for multisample anti-aliasing; defaults to 0; see <https://wiki.libsdl.org/SDL_GLattr#multisample Remarks> for details
multiSampleSamples :: StateVar Int
multiSampleSamples  = attr SDL.glAttrMultiSampleSamples

-- | get\/set @SDL_GL_CONTEXT_MAJOR_VERSION@, OpenGL context major version; see <https://wiki.libsdl.org/SDL_GLattr#OpenGL Remarks> for details 
contextMajorVersion :: StateVar Int
contextMajorVersion = attr SDL.glAttrContextMajorVersion

-- | get\/set @SDL_GL_CONTEXT_MINOR_VERSION@, OpenGL context major version; see <https://wiki.libsdl.org/SDL_GLattr#OpenGL Remarks> for details 
contextMinorVersion :: StateVar Int
contextMinorVersion = attr SDL.glAttrContextMinorVersion

-- | get\/set @SDL_GL_CONTEXT_FLAGS@, some bitwise (.|.) of 0 or more of:
--
-- 'glContextFlagDebug'
--
-- This flag maps to @GLX_CONTEXT_DEBUG_BIT_ARB@ in the @GLX_ARB_create_context@ extension for @X11@ and @WGL_CONTEXT_DEBUG_BIT_ARB@ in the @WGL_ARB_create_context@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to put the GL into a \"debug\" mode which might offer better developer insights, possibly at a loss of performance (although a given GL implementation may or may not do anything differently in the presence of this flag).
--
-- 'glContextFlagForwardCompatible'
--
-- This flag maps to @GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB@ in the @GLX_ARB_create_context@ extension for X11 and @WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB@ in the @WGL_ARB_create_context@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to put the GL into a \"forward compatible\" mode, which means that no deprecated functionality will be supported, possibly at a gain in performance, and only applies to GL 3.0 and later contexts.
--
-- 'glContextFlagResetIsolation'
--
-- This flag maps to @GLX_CONTEXT_RESET_ISOLATION_BIT_ARB@ in the @GLX_ARB_robustness_isolation@ extension for X11 and @WGL_CONTEXT_RESET_ISOLATION_BIT_ARB@ in the @WGL_ARB_create_context_robustness@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to require the GL to make promises about what to do in the face of driver or hardware failure.
--
-- 'glContextFlagRobustAccess'
--
-- This flag maps to @GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB@ in the @GLX_ARB_create_context_robustness@ extension for X11 and @WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB@ in the @WGL_ARB_create_context_robustness@ extension for Windows. This flag is currently ignored on other targets that don't support equivalent functionality. This flag is intended to require a GL context that supports the GL_ARB_robustness extension--a mode that offers a few APIs that are safer than the usual defaults (think @snprintf@() vs @sprintf@()).

contextFlags :: StateVar Int
contextFlags = attr SDL.glAttrContextFlags

-- | get\/set @SDL_GL_CONTEXT_PROFILE_MASK@, which must be _one_ of
--
-- * 'glProfileCore'
--
-- * 'glProfileCompatibility'
--
-- * 'glProfileES'
--
-- Despite the name implying you could (.|.) these together, these are mutually exclusive!

contextProfileMask :: StateVar Int
contextProfileMask  = attr SDL.glAttrContextProfileMask

-- | get\/set @SDL_GL_STEREO@, whether the output is stereo 3D; defaults to off
stereo :: StateVar Bool
stereo = boolAttr SDL.glAttrStereo

-- | get\/set @SDL_GL_ACCELERATED_VISUAL@, set to 'True' to require hardware acceleration, set to 'False' to force software rendering; defaults to allow either
acceleratedVisual :: StateVar Bool
acceleratedVisual = boolAttr SDL.glAttrAcceleratedVisual

-- | get\/set @SDL_GL_DOUBLEBUFFER@
doubleBuffer :: StateVar Bool
doubleBuffer = boolAttr SDL.glAttrDoubleBuffer

-- | get\/set @SDL_GL_SHARE_WITH_CURRENT_CONTEXT@
shareWithCurrentContext :: StateVar Bool
shareWithCurrentContext = boolAttr SDL.glAttrShareWithCurrentContext

-- | get\/set @SDL_GL_FRAMEBUFFER_SRGB_CAPABLE@
framebufferSRGBCapable :: StateVar Bool
framebufferSRGBCapable  = boolAttr SDL.glAttrFramebufferSRGBCapable

windowDisplayMode :: SDL.Window -> StateVar SDL.DisplayMode
windowDisplayMode w = makeStateVar getWDM setWDM where
  getWDM = alloca $ \p -> do
    SDL.getWindowDisplayMode w p >>= err
    peek p 
  setWDM m = alloca $ \p -> do
    poke p m
    SDL.setWindowDisplayMode w p >>= err

desktopDisplayMode :: Int -> IO SDL.DisplayMode
desktopDisplayMode idx = alloca $ \p -> do
  SDL.getDesktopDisplayMode (fromIntegral idx) p >>= err
  peek p

elemOff :: forall a. Storable a => Ptr a -> Int -> Ptr a
elemOff p n = p `plusPtr` (n * sizeOf (undefined :: a))

windowSize :: SDL.Window -> StateVar Size
windowSize win = makeStateVar g s where
 g = allocaArray 2 $ \p -> do
   SDL.getWindowSize win p (elemOff p 1)
   w <- peek p
   h <- peekElemOff p 1
   return $ Size (fromIntegral w) (fromIntegral h)
 s (Size w h) = SDL.setWindowSize win (fromIntegral w) (fromIntegral h)

-- | Abstracts over @SDL_GL_GetSwapInterval@ / @SDL_GL_SetSwapInterval@
--
-- 0 for immediate updates, 1 for updates synchronized with the vertical retrace. -1 for late swap tearing (if supported)
-- late swap tearing support can be checked under the @GLX_EXT_swap_control_tear@ extension

swapInterval :: StateVar Int
swapInterval = makeStateVar (fromIntegral <$> SDL.glGetSwapInterval) (\a -> SDL.glSetSwapInterval (fromIntegral a) >>= err)

-- * Utilities

-- | Use a GLattr as a variable
attr :: SDL.GLattr -> StateVar Int
attr a = makeStateVar (getAttr a) (setAttr a)

boolAttr :: SDL.GLattr -> StateVar Bool
boolAttr = xmap fromEnum toEnum . attr

getAttr :: SDL.GLattr -> IO Int
getAttr a = alloca $ \p -> do
 SDL.glGetAttribute a p >>= err
 fromIntegral <$> peek p

setAttr :: SDL.GLattr -> Int -> IO ()
setAttr a i = SDL.glSetAttribute a (fromIntegral i) >>= err

makeCurrent :: MonadIO m => SDL.Window -> SDL.GLContext -> m ()
makeCurrent w c = liftIO (SDL.glMakeCurrent w c >>= err)

