module UI.RenderRasterific 
  where

import Graphics.Rasterific (renderDrawing, Drawing)
import Codec.Picture (PixelRGBA8)
import qualified GI.Cairo as GIC
import qualified Data.Vector.Storable as VS
import Data.Word (byteSwap32, Word8, Word32)
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Internal as C (runRender, Cairo(..))
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Data.Bits (rotateL, Bits(rotateR))
import Codec.Picture (PixelRGBA8(PixelRGBA8), Image(imageData))
import Foreign.Storable (Storable(..))

-- FIXME: Might depend on endianness

renderRasterific :: Int -> Int -> GIC.Context -> Drawing PixelRGBA8 () -> IO ()
renderRasterific w h context drawing = do
  let img = renderDrawing w h (PixelRGBA8 0 0 0 0) drawing
      imageVector :: VS.Vector Word32 = VS.unsafeCast $ imageData img
  VS.unsafeWith (VS.map (byteSwap32 . flip rotateL 8) imageVector) $ \imgPtr -> do
    C.withImageSurfaceForData (castPtr imgPtr) C.FormatARGB32 w h (C.formatStrideForWidth C.FormatARGB32 w) $ \surface -> do
      runRenderWithContext context $ do
        C.setSourceSurface surface 0 0
        C.newPath
        C.rectangle 0 0 (fromIntegral w) (fromIntegral h)
        C.paint
  pure ()

runRenderWithContext :: GIC.Context -> C.Render () -> IO ()
runRenderWithContext ct r = GIC.withManagedPtr ct $ \p ->
  runReaderT (C.runRender r) (C.Cairo (castPtr p))
