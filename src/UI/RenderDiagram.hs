module UI.RenderDiagram where

import Codec.Picture (PixelRGBA8)
import qualified GI.Cairo as GIC
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Internal as C (runRender, Cairo(..))
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Data.Bits (rotateL, Bits(rotateR))
import Codec.Picture (PixelRGBA8(PixelRGBA8), Image(imageData))
import Foreign.Storable (Storable(..))
import Diagrams.Backend.Cairo.Ptr (renderPtr)
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Core
import Foreign.Marshal.Alloc


renderDiagram :: Int -> Int -> GIC.Context -> Diagram Cairo -> IO ()
renderDiagram w h context diagram = do
  ptr <- renderPtr w h (C.FormatARGB32) diagram
  C.withImageSurfaceForData (castPtr ptr) C.FormatARGB32 w h (C.formatStrideForWidth C.FormatARGB32 w) $ \surface -> do
    runRenderWithContext context $ do
      C.setSourceSurface surface 0 0
      C.newPath
      C.rectangle 0 0 (fromIntegral w) (fromIntegral h)
      C.paint
  free ptr
  pure ()

runRenderWithContext :: GIC.Context -> C.Render () -> IO ()
runRenderWithContext ct r = GIC.withManagedPtr ct $ \p ->
  runReaderT (C.runRender r) (C.Cairo (castPtr p))
