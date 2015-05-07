import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad

import Init
import Display
import Input
import WSC
import Integrators

main :: IO ()
main = do
  (stepSize,initialState) <- loadStartState
  initAndRun $ ourMainLoop getInputState (wsc (useStroemerMethod stepSize)) display initialState


ourMainLoop :: IO b -> (a -> b -> a) -> (a -> IO c) -> a -> IO ()
ourMainLoop inputHandler worldStateComputation drawWorld worldState = loop worldState
  where loop oldState = do
          inputs <- inputHandler
          newState <- return (worldStateComputation oldState inputs)
          drawWorld newState
	  
	  windowOpen <- getParam Opened
          unless (not windowOpen) $
            loop newState

