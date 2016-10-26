module Main where

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO display background stepRate initialModel drawModel handleEvents stepModel
  where
    display = InWindow "SIMPLEX MIPLEX!!1!" (512, 512) (0, 0)
    background = makeColor 1 1 1 1
    stepRate = 1 -- one a second

data Model = Model

initialModel :: Model
initialModel = Model

drawModel :: Model -> IO Picture
drawModel _ = return $ circle 80

handleEvents :: Event -> Model -> IO Model
handleEvents _ m = return m

stepModel :: Float -> Model -> IO Model
stepModel _ m = return m