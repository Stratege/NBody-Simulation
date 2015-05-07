module Init where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))

initAndRun run = do
  initSuccessful <- GLFW.initialize
  -- open window
  windowSuccessful <- GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Pluto is still a planet"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- enable depth buffer
  GL.depthFunc 	$= Just Less
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0


  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
--      GL.ortho 0 (realToFrac w) (realToFrac h) 0 0 (-100)
      GL.perspective 50 ((realToFrac w) / (realToFrac h)) (100.0) (-10000.0)
      GL.lookAt (Vertex3 0 0 3000) (Vertex3 0 0 0) (Vector3 0 (1) 0)

  -- the getKeyboardState and getMouseState functions depend on this being enabled to work properly
  enableSpecial StickyKey
  enableSpecial StickyMouseButton
  enableSpecial SystemKey
  enableSpecial KeyRepeat
  disableSpecial AutoPollEvent
  run

  GLFW.closeWindow
  GLFW.terminate