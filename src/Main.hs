{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Monoid
import Data.Vector (Vector, (!))
import Graphics.Gloss.Data.Picture hiding (Vector)
import Graphics.Gloss.Interface.Pure.Game (Event, play, Picture)
import Graphics.Gloss.Raster.Field
import qualified Data.Vector as V
import Data.Default.Class
import System.IO.Unsafe (unsafePerformIO)

import Optimise (Simplex(..), getSimplexPoints, FminOpts (..), initSimplex, stepSearch)

main :: IO ()
main = playFieldWithPic display cellSize stepRate initialModel drawModel handleEvents stepModel
  where
    display = InWindow "SIMPLEX MIPLEX!!1!" (1024, 1000) (0, 0)
    stepRate = 1 -- one a second
    cellSize = (2,2)

-- | Calculate cost at (x, y)
type CostFunction = Double -> Double -> Double

data Model = Model {
  modelFunction :: !CostFunction
, modelSimplex :: !Simplex
}

scaleFunc :: Double -> CostFunction -> CostFunction
scaleFunc v f = \x y -> f (v * x) (v * y)

initialModel :: Model
initialModel = Model {
    modelFunction = costFunc
  , modelSimplex = let
      FminOpts {..} = def
      ps = unsafePerformIO $ getSimplexPoints 50 [10, 10]
      s = Simplex ps costFunc' [] oAlpha oBetha oGamma oError (([],0),([],0),([],0)) []
      in initSimplex s
  } where
    costFunc = scaleFunc 5 $ \x y -> 10 - x**2 - 4*x + y**2 - y - x*y
    costFunc' [x, y] = costFunc x y

-- | Color gradient for heatmap
colorGradient :: Vector Color
colorGradient = V.fromList [
    rgbI 242 204 242
  , rgbI 232 164 232
  , rgbI 222 124 222
  , rgbI 204 106 216
  , rgbI 174 115 216
  , rgbI 145 125 216
  , rgbI 103 115 224
  , rgbI 58 100 234
  , rgbI 37 103 241
  , rgbI 87 163 236
  , rgbI 136 222 231
  , rgbI 129 229 199
  , rgbI 95 209 155
  , rgbI 95 209 155
  , rgbI 47 202 82
  , rgbI 32 213 53
  , rgbI 47 218 50
  , rgbI 92 217 75
  , rgbI 136 216 100
  , rgbI 176 225 65
  , rgbI 215 235 26
  , rgbI 243 232 1
  , rgbI 248 202 6
  , rgbI 253 172 11
  , rgbI 250 28 42
  , rgbI 255 58 76
  , rgbI 255 137 121
  , rgbI 255 216 165
  ]

-- | Convert function value into color with gradient
pickColorGradient :: Double -- ^ Min value
  -> Double -- ^ Max value
  -> Double -- ^ Function value
  -> Color
pickColorGradient minv maxv v = mixColors di (1-di) highColor lowColor
  where
    n = V.length colorGradient
    nv = (v - minv)/(maxv-minv)
    i = nv * fromIntegral n
    clamp = max 0 . min (n-1)
    lowI = clamp $ floor i
    lowColor = colorGradient ! lowI
    highI = clamp $ ceiling i
    highColor = colorGradient ! highI
    di = realToFrac $ i - fromIntegral lowI

drawModel :: Model -> (Point -> Color, Picture)
drawModel Model{..} = (makeBackground, drawSimplex modelSimplex)
  where
    makeBackground (x, y) = pickColorGradient (-50) 50 $ modelFunction (realToFrac x) (realToFrac y)

handleEvents :: Event -> Model -> Model
handleEvents _ m = m

stepModel :: Float -> Model -> Model
stepModel _ m = m { modelSimplex = stepSearch $ modelSimplex m }

drawSimplex :: Simplex -> Picture
drawSimplex Simplex{..} = lineLoop $ fmap (\[x, y] -> (realToFrac x, realToFrac y)) sPoints

-- HA-HA-HA my fork of the 'playField' function
-- | Play a game with a continous 2D function.
playFieldWithPic
  :: Display                   -- ^ Display mode.
  -> (Int, Int)                -- ^ Number of pixels to draw per point.
  -> Int                       -- ^ Number of simulation steps to take
                               --   for each second of real time
  -> world                     -- ^ The initial world.
  -> (world -> (Point -> Color, Picture)) -- ^ Function to compute the color of the world at the given point.
  -> (Event -> world -> world) -- ^ Function to handle input events.
  -> (Float -> world -> world) -- ^ Function to step the world one iteration.
                               --   It is passed the time in seconds since the program started.
  -> IO ()
playFieldWithPic !display (!zoomX, !zoomY) !stepRate !initWorld !drawWorld !handleEvent !stepWorld =
  if zoomX < 1 || zoomY < 1
  then error $ "Graphics.Gloss.Raster.Field: invalid pixel scale factor " ++ show (zoomX, zoomY)
  else let
       (!winSizeX, !winSizeY) = sizeOfDisplay display
    in play display black stepRate
            initWorld
            (\ !world -> let
              (makePixel, pic) = drawWorld world
              in makePicture winSizeX winSizeY zoomX zoomY makePixel <> pic)
            handleEvent
            stepWorld
{-# INLINE playFieldWithPic #-}

sizeOfDisplay :: Display -> (Int, Int)
sizeOfDisplay display
 = case display of
        InWindow _ s _  -> s
        FullScreen s    -> s
{-# INLINE sizeOfDisplay #-}