module Graphics.GD (
                    -- * Types
                    Image, Size, Point, Color,
                    -- * Creating and copying images
                    newImage, copyImage,
                    copyRegion, copyRegionScaled,
                    -- * Memory management
                    withImage,
                    -- * Loading images
                    -- ** JPEG
                    loadJpegFile, loadJpegData, loadJpegByteString,
                    -- ** PNG
                    loadPngFile, loadPngData, loadPngByteString,
                    -- ** GIF
                    loadGifFile, loadGifData, loadGifByteString,
                    -- * Saving images
                    -- ** JPEG
                    saveJpegFile, saveJpegByteString,
                    -- ** PNG
                    savePngFile, savePngByteString,
                    -- ** GIF
                    saveGifFile, saveGifByteString,
                    -- * Getting image information
                    imageSize,
                    -- * Manipulating images
                    resizeImage, rotateImage,
                    -- * Drawing
                    fillImage,
                    drawFilledRectangle,
                    drawFilledEllipse,
                    drawLine,
                    drawArc,
                    antiAliased,
                    getPixel,
                    setPixel,
                    -- * Text
                    useFontConfig,
                    drawString, measureString,
                    drawStringCircle,
                    -- * Colors
                    rgb, rgba, unRgba
                   ) where

import Control.Exception (bracket)
import Control.Monad (liftM, unless)
import qualified Data.ByteString.Internal as B
import Foreign
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Error
import System.IO.Error

data CFILE = CFILE

foreign import ccall "stdio.h fopen" c_fopen
    :: CString -> CString -> IO (Ptr CFILE)
foreign import ccall "stdio.h fclose" c_fclose
    :: Ptr CFILE -> IO CInt

fopen :: FilePath -> String -> IO (Ptr CFILE)
fopen file mode =
    throwErrnoIfNull file $ withCString file $
                         \f -> withCString mode $ \m -> c_fopen f m

fclose :: Ptr CFILE -> IO ()
fclose p = throwErrnoIf_ (== #{const EOF}) "fclose" $ c_fclose p

withCFILE :: FilePath -> String -> (Ptr CFILE -> IO a) -> IO a
withCFILE file mode = bracket (fopen file mode) fclose

#include <gd.h>
#include "gd-extras.h"

data GDImage = GDImage

-- JPEG format

foreign import ccall "gd.h gdImageCreateFromJpeg" gdImageCreateFromJpeg
    :: Ptr CFILE -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageCreateFromJpegPtr" gdImageCreateFromJpegPtr
    :: CInt -> Ptr a -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageJpeg" gdImageJpeg
    :: Ptr GDImage -> Ptr CFILE -> CInt -> IO ()

foreign import ccall "gd.h gdImageJpegPtr" gdImageJpegPtr
    :: Ptr GDImage -> Ptr CInt -> CInt -> IO (Ptr a)

-- PNG format

foreign import ccall "gd.h gdImageCreateFromPng" gdImageCreateFromPng
    :: Ptr CFILE -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageCreateFromPngPtr" gdImageCreateFromPngPtr
    :: CInt -> Ptr a -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImagePng" gdImagePng
    :: Ptr GDImage -> Ptr CFILE -> IO ()

foreign import ccall "gd.h gdImagePngPtr" gdImagePngPtr
    :: Ptr GDImage -> Ptr CInt -> IO (Ptr a)

-- GIF format

foreign import ccall "gd.h gdImageCreateFromGif" gdImageCreateFromGif
    :: Ptr CFILE -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageCreateFromGifPtr" gdImageCreateFromGifPtr
    :: CInt -> Ptr a -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageGif" gdImageGif
    :: Ptr GDImage -> Ptr CFILE -> IO ()

foreign import ccall "gd.h gdImageGifPtr" gdImageGifPtr
    :: Ptr GDImage -> Ptr CInt -> IO (Ptr a)

-- Creating and destroying images

foreign import ccall "gd.h gdImageCreateTrueColor" gdImageCreateTrueColor
    :: CInt -> CInt -> IO (Ptr GDImage)

foreign import ccall "gd.h gdImageDestroy" gdImageDestroy
    :: Ptr GDImage -> IO ()

foreign import ccall "gd-extras.h &gdImagePtrDestroyIfNotNull" ptr_gdImagePtrDestroyIfNotNull
    :: FunPtr (Ptr (Ptr GDImage) -> IO ())


-- Copying image parts

foreign import ccall "gd.h gdImageCopy" gdImageCopy
    :: Ptr GDImage -> Ptr GDImage
    -> CInt -> CInt -> CInt -> CInt
    -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageCopyResampled" gdImageCopyResampled
    :: Ptr GDImage -> Ptr GDImage
    -> CInt -> CInt -> CInt -> CInt
    -> CInt -> CInt -> CInt -> CInt -> IO ()

{-
foreign import ccall "gd.h gdImageCopyRotated" gdImageCopyRotated
    :: Ptr GDImage -> Ptr GDImage
    -> CDouble -> CDouble -> CInt -> CInt
    -> CInt -> CInt -> CInt -> IO ()
-}

foreign import ccall "gd-extras.h gdImageCopyRotated90" gdImageCopyRotated90
    :: Ptr GDImage -> Ptr GDImage
    -> CInt -> CInt -> CInt -> CInt
    -> CInt -> CInt -> CInt -> IO ()


-- Drawing functions

foreign import ccall "gd.h gdImageFilledRectangle" gdImageFilledRectangle
    :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageFilledEllipse" gdImageFilledEllipse
    :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageLine" gdImageLine
    :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageArc" gdImageArc
    :: Ptr GDImage -> CInt -> CInt -> CInt -> CInt
    -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "gd.h gdImageSetAntiAliased" gdImageSetAntiAliased
    :: Ptr GDImage -> CInt -> IO ()

foreign import ccall "gd.h gdImageGetPixel" gdImageGetPixel
    :: Ptr GDImage -> CInt -> CInt -> IO CInt

foreign import ccall "gd.h gdImageSetPixel" gdImageSetPixel
    :: Ptr GDImage -> CInt -> CInt -> CInt -> IO ()

-- Text functions

foreign import ccall "gd.h gdFTUseFontConfig" gdFTUseFontConfig
    :: CInt -> IO CInt

foreign import ccall "gd.h gdImageStringFT" gdImageStringFT
    :: Ptr GDImage -> Ptr CInt -> CInt -> CString -> CDouble -> CDouble -> CInt -> CInt -> CString -> IO CString

foreign import ccall "gd.h gdImageStringFTCircle" gdImageStringFTCircle
    :: Ptr GDImage -> CInt -> CInt -> CDouble -> CDouble -> CDouble -> CString -> CDouble -> CString -> CString -> CInt -> IO CString

-- Miscellaneous functions

foreign import ccall "gd.h &gdFree" gdFree
    :: FunPtr (Ptr a -> IO ())

-- We use a second level of indirection to allow storing a null pointer
-- when the image has already been freed. This allows 'withImage' to
-- free the @gdImage@ early.
newtype Image = Image (ForeignPtr (Ptr GDImage))

type Size = (Int,Int)

type Point = (Int,Int)

type Color = CInt

mkImage :: Ptr GDImage -> IO Image
mkImage img = do fp <- mallocForeignPtr
                 withForeignPtr fp $ \p -> poke p img
                 addForeignPtrFinalizer ptr_gdImagePtrDestroyIfNotNull fp
                 return $ Image fp

-- | Creates an image, performs an operation on the image, and
-- frees it.
-- This function allows block scoped management of 'Image' objects.
-- If you are handling large images, the delay before the finalizer which frees
-- the image runs may cause significant temporary extra memory use.
-- Use this function to force the image to be freed as soons as you are done with it.
-- Note that it is unsafe to hold on to the 'Image' after the
-- function is done.
withImage :: IO Image -- ^ Image creation action.
          -> (Image -> IO b) -- ^ Some operation on the image. The result should
                             -- not reference the 'Image'.
          -> IO b
withImage ini f = bracket ini freeImage f

-- | Overwrites the pointer with a null pointer, and frees the @gdImage@.
-- Safe to call twice. Doesn't free the 'ForeignPtr', we rely on the
-- GC to do that.
freeImage :: Image -> IO ()
freeImage (Image fp) = withForeignPtr fp $
  \pp -> do p <- peek pp
            poke pp nullPtr
            unless (p == nullPtr) $ gdImageDestroy p

withImagePtr :: Image -> (Ptr GDImage -> IO a) -> IO a
withImagePtr (Image fp) f = withForeignPtr fp $
  \pp -> peek pp >>= \p -> if p == nullPtr then fail "Image has been freed." else f p

-- | Create a new empty image.
newImage :: Size -> IO Image
newImage (w,h) = newImage_ (int w) (int h)

newImage_  :: CInt -> CInt -> IO Image
newImage_ w h = do p <- throwIfNull "gdImageCreateTrueColor" $
                        gdImageCreateTrueColor w h
                   mkImage p

-- | Create a new empty image and apply a function to it.
onNewImage :: CInt -> CInt -> (Ptr GDImage -> IO a) -> IO Image
onNewImage w h f = newImage_ w h >>= \i -> withImagePtr i f >> return i

-- | Make a copy of an image.
copyImage :: Image -> IO Image
copyImage i = withImagePtr i f
  where f p = do (w,h) <- imageSize_ p
                 onNewImage w h (\p' -> gdImageCopy p' p 0 0 0 0 w h)

-- | Copy a region of one image into another
copyRegion :: Point -- ^ Source upper left-hand corner
              -> Size -- ^ Size of copied region
              -> Image -- ^ Source image
              -> Point -- ^ Destination upper left-hand corner
              -> Image -- ^ Destination image
              -> IO ()
copyRegion (srcX, srcY) (w, h) srcIPtr (dstX, dstY) dstIPtr
    = withImagePtr dstIPtr $
      \dstImg -> withImagePtr srcIPtr $
      \srcImg -> gdImageCopy dstImg srcImg (int dstX) (int dstY) (int srcX) (int srcY) (int w) (int h)

-- | Copy a region of one image into another, rescaling the region
copyRegionScaled :: Point -- ^ Source upper left-hand corner
                 -> Size -- ^ Size of source region
                 -> Image -- ^ Source image
                 -> Point -- ^ Destination upper left-hand corner
                 -> Size -- ^ Size of destination region
                 -> Image -- ^ Destination image
                 -> IO ()
copyRegionScaled (srcX, srcY) (srcW, srcH) srcIPtr (dstX, dstY) (dstW, dstH) dstIPtr
    = withImagePtr dstIPtr $
      \dstImg -> withImagePtr srcIPtr $
      \srcImg -> gdImageCopyResampled dstImg srcImg (int dstX) (int dstY) (int srcX) (int srcY) (int dstW) (int dstH) (int srcW) (int srcH)

--
-- * Loading images
--

-- | Load a JPEG image from a file.
loadJpegFile :: FilePath -> IO Image
loadJpegFile = loadImageFile gdImageCreateFromJpeg

-- | Load a JPEG image from a buffer.
loadJpegData :: Int -- ^ Buffer size.
             -> Ptr a -- ^ Buffer with image data.
             -> IO Image
loadJpegData = loadImageData gdImageCreateFromJpegPtr

-- | Load a JPEG image from a ByteString
loadJpegByteString :: B.ByteString -> IO Image
loadJpegByteString = onByteStringData loadJpegData


-- | Load a PNG image from a file.
loadPngFile :: FilePath -> IO Image
loadPngFile = loadImageFile gdImageCreateFromPng

-- | Load a PNG image from a buffer.
loadPngData :: Int -- ^ Buffer size.
            -> Ptr a -- ^ Buffer with image data.
            -> IO Image
loadPngData = loadImageData gdImageCreateFromPngPtr

-- | Load a PNG image from a ByteString
loadPngByteString :: B.ByteString -> IO Image
loadPngByteString = onByteStringData loadPngData

-- | Load a GIF image from a file.
loadGifFile :: FilePath -> IO Image
loadGifFile = loadImageFile gdImageCreateFromGif

-- | Load a GIF image from a buffer.
loadGifData :: Int -- ^ Buffer size.
            -> Ptr a -- ^ Buffer with image data.
            -> IO Image
loadGifData = loadImageData gdImageCreateFromGifPtr

-- | Load a GIF image from a ByteString
loadGifByteString :: B.ByteString -> IO Image
loadGifByteString = onByteStringData loadGifData


loadImageFile :: (Ptr CFILE -> IO (Ptr GDImage)) -> FilePath -> IO Image
loadImageFile f file =
    do p <- throwIfNull ("Loading image from " ++ file) $ withCFILE file "rb" f
       mkImage p

loadImageData :: (CInt -> Ptr a -> IO (Ptr GDImage)) -> Int -> Ptr a -> IO Image
loadImageData f sz buf =
    do p <- throwIfNull ("Loading image") $ f (fromIntegral sz) buf
       mkImage p

onByteStringData :: (Int -> Ptr a -> IO b) -> B.ByteString -> IO b
onByteStringData f bstr
    = case B.toForeignPtr bstr of
        (fptr, start, sz) -> withForeignPtr fptr (\ptr -> f sz (plusPtr ptr start))

--
-- * Saving images
--

-- | Save an image as a JPEG file.
saveJpegFile :: Int -- ^ quality: 0-95, or negative for default quality.
             -> FilePath -> Image -> IO ()
saveJpegFile q = saveImageFile (\p h -> gdImageJpeg p h (fromIntegral q))

-- | Write a JPEG format ByteString of an image.
saveJpegByteString :: Int -> Image -> IO B.ByteString
saveJpegByteString q = saveImageByteString (\p h -> gdImageJpegPtr p h (fromIntegral q))


-- | Save an image as a PNG file.
savePngFile :: FilePath -> Image -> IO ()
savePngFile = saveImageFile gdImagePng

-- | Write a PNG format ByteString of an image.
savePngByteString :: Image -> IO B.ByteString
savePngByteString = saveImageByteString gdImagePngPtr


-- | Save an image as a GIF file.
saveGifFile :: FilePath -> Image -> IO ()
saveGifFile = saveImageFile gdImageGif

-- | Write a GIF format ByteString of an image.
saveGifByteString :: Image -> IO B.ByteString
saveGifByteString = saveImageByteString gdImageGifPtr

saveImageFile :: (Ptr GDImage -> Ptr CFILE -> IO ()) -> FilePath -> Image -> IO ()
saveImageFile f file i = withImagePtr i (\p -> withCFILE file "wb" (f p))

saveImageByteString :: (Ptr GDImage -> Ptr CInt -> IO (Ptr a)) -> Image -> IO (B.ByteString)
saveImageByteString f img = withImagePtr img (\p -> dataByteString (f p))

dataByteString :: (Ptr CInt -> IO (Ptr a)) -> IO B.ByteString
dataByteString f = alloca $ \szPtr -> do datPtr <- f szPtr >>= newForeignPtr gdFree . castPtr
                                         liftM (B.fromForeignPtr datPtr 0 . fromIntegral) (peek szPtr)

--
-- * Getting information about images.
--

-- | Get the size of an image.
imageSize :: Image -> IO (Int,Int) -- ^ (width, height)
imageSize i = liftM f $ withImagePtr i imageSize_
    where f = (\ (w,h) -> (fromIntegral w, fromIntegral h))

imageSize_ :: Ptr GDImage -> IO (CInt,CInt)
imageSize_ p = do w <- #{peek gdImage, sx} p
                  h <- #{peek gdImage, sy} p
                  return (w, h)

--
-- * Transforming images.
--

-- | Resize an image to a give size.
resizeImage :: Int -- ^ width in pixels of output image
            -> Int -- ^ height in pixels of output image
            -> Image
            -> IO Image
resizeImage w h i = withImagePtr i f
    where
      f p = do let (outW,outH) = (fromIntegral w, fromIntegral h)
               (inW, inH) <- imageSize_ p
               onNewImage outW outH $ \p' ->
                   gdImageCopyResampled p' p 0 0 0 0 outW outH inW inH

-- | Rotate an image by a multiple of 90 degrees counter-clockwise.
rotateImage :: Int -- ^ 1 for 90 degrees counter-clockwise,
                   -- 2 for 180 degrees, etc.
            -> Image
            -> IO Image
rotateImage r i = withImagePtr i f
    where f p = do (inW,inH) <- imageSize_ p
                   let q = fromIntegral (r `mod` 4)
                       (outW,outH) | r `mod` 2 == 0 = (inW,inH)
                                   | otherwise      = (inH,inW)
                       srcX = if q == 1 || q == 2 then inW-1 else 0;
                       srcY = if q == 2 || q == 3 then inH-1 else 0;
                   onNewImage outW outH (\p' ->
                       gdImageCopyRotated90 p' p 0 0 srcX srcY inW inH q)

--
-- * Drawing
--

-- | Fill the entire image with the given color.
fillImage :: Color -> Image -> IO ()
fillImage c i = do sz <- imageSize i
                   drawFilledRectangle (0,0) sz c i

drawFilledRectangle :: Point -- ^ Upper left corner
                    -> Point -- ^ Lower right corner
                    -> Color -> Image -> IO ()
drawFilledRectangle (x1,y1) (x2,y2) c i =
    withImagePtr i $ \p ->
        gdImageFilledRectangle p (int x1) (int y1) (int x2) (int y2) c

drawFilledEllipse :: Point -- ^ Center
                  -> Size  -- ^ Width and height
                  -> Color -> Image -> IO ()
drawFilledEllipse (cx,cy) (w,h) c i =
    withImagePtr i $ \p ->
        gdImageFilledEllipse p (int cx) (int cy) (int w) (int h) c

drawLine :: Point -- ^ Start
         -> Point -- ^ End
         -> Color -> Image -> IO ()
drawLine (x1,y1) (x2,y2) c i =
    withImagePtr i $ \p ->
        gdImageLine p (int x1) (int y1) (int x2) (int y2) c

drawArc :: Point -- ^ Center
        -> Size  -- ^ Width and height
        -> Int   -- ^ Starting position (degrees)
        -> Int   -- ^ Ending position (degrees)
        -> Color -> Image -> IO ()
drawArc (cx,cy) (w,h) sp ep c i =
    withImagePtr i $ \p ->
        gdImageArc p (int cx) (int cy) (int w) (int h) (int sp) (int ep) c

-- | Use anti-aliasing when performing the given drawing function.
--   This can cause a segault with some gd versions.
antiAliased :: (Color -> Image -> IO a) -> Color -> Image -> IO a
antiAliased f c i =
    do withImagePtr i (\p -> gdImageSetAntiAliased p c)
       f (#{const gdAntiAliased}) i

getPixel :: Point -> Image -> IO Color
getPixel (x,y) i =
    withImagePtr i $ \p ->
        gdImageGetPixel p (int x) (int y)

setPixel :: Point -> Color -> Image -> IO ()
setPixel (x,y) c i =
    withImagePtr i $ \p ->
        gdImageSetPixel p (int x) (int y) c

--
-- * Text
--

-- | Globally switch from using font file names to fontconfig paths
-- | for fonts in drawString (and measureString).
useFontConfig :: Bool -> IO Bool
useFontConfig use = liftM (/= 0) $ gdFTUseFontConfig $ if use then 1 else 0

-- | Draw a string using the FreeType 2.x library
drawString :: String -- ^ Font name
           -> Double -- ^ Font point size
           -> Double -- ^ Angle in counterclockwise radians
           -> Point -- ^ Origin
           -> String -- ^ Text, including HTML entities
           -> Color -> Image -> IO (Point, Point, Point, Point) -- ^ Bounding box of the drawn text
drawString fontName ptSize angle (oriX, oriY) txt color img
    = withImagePtr img $ drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt

-- | Measure a string using the FreeType 2.x library.  This computes
-- the bounding box but does not actually draw the string to any
-- image.
measureString :: String -- ^ Font name
              -> Double -- ^ Font point size
              -> Double -- ^ Angle in counterclockwise radians
              -> Point -- ^ Origin
              -> String -- ^ Text, including HTML entities
              -> Color -> IO (Point, Point, Point, Point) -- ^ Bounding box of the drawn text
measureString fontName ptSize angle (oriX, oriY) txt color
    = drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt nullPtr

drawStringImagePtr :: Color -> String -> Double -> Double -> Point -> String -> Ptr GDImage -> IO (Point, Point, Point, Point)
drawStringImagePtr color fontName ptSize angle (oriX, oriY) txt imgPtr
    = allocaArray 8 $
      \bboxPtr -> withCAString fontName $
      \cFontName -> withCAString txt $
      \cTxt -> do res <- gdImageStringFT imgPtr bboxPtr color cFontName (double ptSize) (double angle) (int oriX) (int oriY) cTxt
                  if res == nullPtr
                     then peekArray 8 bboxPtr >>= parseBBox
                     else peekCAString res >>= ioError . userError
    where parseBBox l = case map int l of
                          [llx, lly, lrx, lry, urx, ury, ulx, uly] -> return ((llx, lly), (lrx, lry), (urx, ury), (ulx, uly))
                          _ -> ioError $ userError $ "parseBBox with /= 8 elements: " ++ show l

-- | Draw strings around the top and bottom of a torus
drawStringCircle :: Point -- ^ Center of text path circle
                 -> Double -- ^ Outer radius of text
                 -> Double -- ^ Fraction of radius occupied by text
                 -> Double -- ^ Portion of circle arc filled by text
                 -> String -- ^ Font name
                 -> Double -- ^ Font size hint
                 -> String -- ^ Text to write on the top of the circle
                 -> String -- ^ Text to write on the bottom of the circle
                 -> Color -- ^ Text color
                 -> Image -> IO ()
drawStringCircle (ctrX, ctrY) rad textRad textFill fontName fontSize topTxt bottomTxt color img
    = withCAString fontName $
      \cFontName -> withCAString topTxt $
      \cTopTxt -> withCAString bottomTxt $
      \cBottomTxt -> withImagePtr img $
      \imgPtr -> do res <- gdImageStringFTCircle imgPtr (int ctrX) (int ctrY) (double rad) (double textRad) (double textFill) cFontName (double fontSize) cTopTxt cBottomTxt  color
                    unless (res == nullPtr) (peekCAString res >>= ioError . userError)

--
-- * Colors
--

rgb :: Int -- ^ Red (0-255)
         -> Int -- ^ Green (0-255)
         -> Int -- ^ Blue (0-255)
         -> Color
rgb r g b = rgba r g b 0

rgba :: Int -- ^ Red (0-255)
          -> Int -- ^ Green (0-255)
          -> Int -- ^ Blue (0-255)
          -> Int -- ^ Alpha (0-127), 0 is opaque, 127 is transparent
          -> Color
rgba r g b a =
    (int a `shiftL` 24) .|.
    (int r `shiftL` 16) .|.
    (int g `shiftL` 8)  .|.
    int b

unRgba :: Color -> (Int, Int, Int, Int)
unRgba c = (int r, int g, int b, int a) where
  a = c3 `shiftR` 8
  r = c3 .&. 255
  g = c2 .&. 255
  b = c  .&. 255
  c2 = c `shiftR` 8
  c3 = c2 `shiftR` 8

--
-- * Utilities
--

int :: (Integral a, Num b) => a -> b
int = fromIntegral

double :: (Real a, Fractional b) => a -> b
double = realToFrac
