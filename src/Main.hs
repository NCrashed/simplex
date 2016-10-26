{-# LANGUAGE RecordWildCards #-}
module Main where

import Graphics.Gloss.Interface.Pure.Game (Event)
import Graphics.Gloss.Raster.Field
import Data.Vector as V

import Debug.Trace

main :: IO ()
main = playField display cellSize stepRate initialModel drawModel handleEvents stepModel
  where
    display = InWindow "SIMPLEX MIPLEX!!1!" (512, 512) (0, 0)
    stepRate = 1 -- one a second
    cellSize = (10,10)

-- | Calculate cost at (x, y)
type CostFunction = Double -> Double -> Double

data Model = Model {
  modelFunction :: !CostFunction
}

initialModel :: Model
initialModel = Model {
    modelFunction = \x y -> 10 - x**2 - 4*x + y**2 - y - x*y
  }

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
pickColorGradient minv maxv v = traceShow highI $ mixColors di (1-di) lowColor highColor
  where
    n = V.length colorGradient
    nv = (v - minv)/(maxv-minv)
    i = nv * fromIntegral n
    lowI = 0 `max` floor i
    lowColor = colorGradient ! lowI
    highI = (n-1) `min` ceiling i
    highColor = colorGradient ! highI
    di = realToFrac $ i - fromIntegral lowI

drawModel :: Model -> Point -> Color
drawModel Model{..} (x, y) = pickColorGradient (-10) 10 v
  where
    v = modelFunction (realToFrac x) (realToFrac y)

handleEvents :: Event -> Model -> Model
handleEvents _ m = m

stepModel :: Float -> Model -> Model
stepModel _ m = m