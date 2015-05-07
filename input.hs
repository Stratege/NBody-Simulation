module Input where
--( Keyboard(..)
--, getInputState
--) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.Char (ord)
import DataStructures

getInputState = do
  pollEvents
  keyboard <- getKeyboardState
  mouse <- getMouseState
  return (keyboard,mouse)

getKeyboardState :: IO Keyboard
getKeyboardState = do --note the letters need to be capital letters
  a <- GLFW.getKey $ ord 'A'
  b <- GLFW.getKey $ ord 'B'
  c <- GLFW.getKey $ ord 'C'
  d <- GLFW.getKey $ ord 'D'
  e <- GLFW.getKey $ ord 'E'
  f <- GLFW.getKey $ ord 'F'
  g <- GLFW.getKey $ ord 'G'
  h <- GLFW.getKey $ ord 'H'
  i <- GLFW.getKey $ ord 'I'
  j <- GLFW.getKey $ ord 'J'
  k <- GLFW.getKey $ ord 'K'
  l <- GLFW.getKey $ ord 'L'
  m <- GLFW.getKey $ ord 'M'
  n <- GLFW.getKey $ ord 'N'
  o <- GLFW.getKey $ ord 'O'
  p <- GLFW.getKey $ ord 'P'
  q <- GLFW.getKey $ ord 'Q'
  r <- GLFW.getKey $ ord 'R'
  s <- GLFW.getKey $ ord 'S'
  t <- GLFW.getKey $ ord 'T'
  u <- GLFW.getKey $ ord 'U'
  v <- GLFW.getKey $ ord 'V'
  w <- GLFW.getKey $ ord 'W'
  x <- GLFW.getKey $ ord 'X'
  y <- GLFW.getKey $ ord 'Y'
  z <- GLFW.getKey $ ord 'Z'
  num0 <- GLFW.getKey $ ord '0'
  num1 <- GLFW.getKey $ ord '1'
  num2 <- GLFW.getKey $ ord '2'
  num3 <- GLFW.getKey $ ord '3'
  num4 <- GLFW.getKey $ ord '4'
  num5 <- GLFW.getKey $ ord '5'
  num6 <- GLFW.getKey $ ord '6'
  num7 <- GLFW.getKey $ ord '7'
  num8 <- GLFW.getKey $ ord '8'
  num9 <- GLFW.getKey $ ord '9'
  return $ Keyboard a b c d e f g h i j k l m n o p q r s t u v w x y z num0 num1 num2 num3 num4 num5 num6 num7 num8 num9

getMouseState :: IO Mouse
getMouseState = do
  pos <- GL.get GLFW.mousePos
  l <- GLFW.getMouseButton GLFW.ButtonLeft
  r <- GLFW.getMouseButton GLFW.ButtonRight
  return $ Mouse pos l r
