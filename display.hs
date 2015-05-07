module Display where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import DataStructures as DS
import Data.List hiding (concat, foldl, foldl')
import Numeric.Units.Dimensional.Prelude ((*~), meter)
import qualified Data.Sequence as Seq
import Data.Foldable
import Prelude hiding (concat, foldl, mapM_)

sfactor = 5e-9 -- 5e-9

display :: (Int, Int, Seq.Seq Object, ObjPosition,a) -> IO ()
display (viewObjNum,zoomLevel,objects,cam@(Vector3 x y z),_) = do
  fpsLimiter
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.color $ color3 1 0 0
  GL.preservingMatrix $ do
--    GL.rotate (90 :: GLdouble) (Vector3 0 1 0)
    GL.translate drawPos -- (vectorDeMeterize cam)
    foldl' (\acc obj -> colorFromMass obj >> renderBody adjustedZoom obj >> acc) (return ()) objects
--    renderLayer (Vector3 (100) (-200) (100))
  GLFW.swapBuffers
  where drawPos = vectorDeMeterize earthPos -- cam -- $ vectorAdd cam earthPos
	earthPos = (\vec -> vectorTimesScalar vec (DS.num (-adjustedZoom))) . getPos . (flip Seq.index (viewObjNum-1)) $ objects 
	adjustedZoom = 5 * (2^^zoomLevel)

--display :: (a, [Point], DS.Sphere, ObjPosition) -> IO ()
--display (_,lines,sphere,cam@(Vector3 x y z)) = do
--  fpsLimiter
--  GL.clear [GL.ColorBuffer]
--  GL.preservingMatrix $ do
--    GL.translate $ (vectorDeMeterize cam)
--    renderLines lines
--    renderSphere sphere
--    renderLayer (Vector3 (100) (-200) (100))
--  GLFW.swapBuffers

colorFromMass (Object _ _ _ _ m)
	| dm > 1e27 = GL.color $ color3 1 1 0
	| dm > 1e24 = GL.color $ color3 0 0 1
	| dm > 1e23 = GL.color $ color3 1 0 0
	| otherwise = GL.color $ color3 (0.5) (0.5) (0.5)
	where dm = deMass m

fpsLimiter = GLFW.sleep 0.01

renderLines :: [Point] -> IO ()
renderLines l = do
  GL.renderPrimitive GL.Lines $ mapM_
      (\ (Vector3 x y z) -> GL.vertex (GL.Vertex3 x y z)) l


color3 :: GLdouble -> GLdouble -> GLdouble -> GL.Color3 GLdouble
color3 = GL.Color3

renderLayer (Vector3 x y z) = renderLines points
	where	points = [(Vector3 (a-(0.5*x)) y (b-(0.5*z))) | a <- widthPoints, b <- heightPoints]
		widthPoints = map (*(width/precision)) [0..precision]
		heightPoints = map (*(height/precision)) [0..precision]
		height = 100
		width = 100
		precision = 40

--this function is bugged but cooooool looking
renderSphere :: DS.Sphere -> IO()
renderSphere (DS.Sphere obj r) = renderLines circleAsLines
			--where   circleAsLines = totalSphere100
			--where   circleAsLines = map (\(Vector3 a b c) -> (Vector3 (x + (a ur)) (y + (b ur)) (z + (c ur)))) totalSphere
			--where   circleAsLines = map (\(Vector3 a b c) -> (Vector3 (a x ur) (b y ur) (c z ur))) wrongSphere
			where 	circleAsLines = sphere
				sphere = concat . map (\(theta,circle) -> map (\(Vector3 x y z) -> (Vector3 (x + ur * sin theta) y (z + ur*cos theta))) circle) . map (\x -> (x,circle)) $ [1..precision]
				circle = map (\theta -> (Vector3 (sizeFactor*x + ur*sin theta) (sizeFactor*y + ur*cos theta) (sizeFactor*z + ur * sin theta))) thetaList
				thetaList = map (*(2*glpi/precision)) [1..precision]
				precision = 40
				ur = deMeterize r
				Vector3 x y z = vectorDeMeterize . getPos $ obj
				sizeFactor = sfactor

renderBody sizeFactor obj = renderLines $ map(\(Vector3 a b c) -> (Vector3 (a+sizeFactor*x) (b+sizeFactor*y) (c+sizeFactor*z))) body
	where 	Vector3 x y z = vectorDeMeterize . getPos $ obj

body = sphere
	where	sphere = concat . map (\(theta,circle) -> map (\(Vector3 x y z) -> (Vector3 (x + ur * sin theta) y (z + ur*cos theta))) circle) . map (\x -> (x,circle)) $ [1..precision]
		circle = map (\theta -> (Vector3 (ur*sin theta) (ur*cos theta) (ur * sin theta))) thetaList
		thetaList = map (*(2*glpi/precision)) [1..precision]
		precision = 40
		ur = 50


				
--wrongSphere = sphere
--	where   sphere =  concat . map (\(theta,circle) -> foo theta circle) . map (\x -> (x,circle)) $ [1..precision]
--		foo theta vec = map (\(Vector3 x y z) -> (Vector3 (\a b -> a + (x b + b * sin theta)) (\a b -> a + (y b + b * cos theta)) (\a b -> a+z b))) vec
--		circle = map (\theta -> (Vector3 (*sin theta) (*cos theta) (*0))) thetaList
--		thetaList = map (*(2*glpi/precision)) [1..precision]
--		precision = 80

--totalSphere100 = map (\(Vector3 x y z) -> (Vector3 (x 100) (y 100) (z 100))) totalSphere

--totalSphere = circle
--	where   sphere = concat . map (\(theta, circles) -> foo theta circles) . map (\x -> (x,circle)) $ [1..precision]
--		foo theta = map (\(Vector3 x y z) -> (Vector3 (\a -> x $ a * cos theta) (\a -> y $ a * sin theta) (\a -> z a)))
--		circle = map (\theta -> (Vector3 (*cos theta) (*cos theta) (*sin theta))) thetaList
--		thetaList = map (*(2*glpi/precision)) [1..precision]
--		precision = 40

--weirdBody = thing
--	where   thing = concat . map (\(theta,circle) -> map (\(Vector3 x y z) -> (Vector3 (x + ur * sin theta) y (z + ur*cos theta))) circle) . map (\x -> (x,circle)) $ [1..precision]
--		circle = map (\theta -> (Vector3 (x + ur*sin theta) (y + ur*cos theta) (z + ur * sin theta))) thetaList
--		thetaList = map (*(2*glpi/precision)) [1..precision]
--		precision = 40
--		ur = 100
--		Vector3 x y z = (Vector3 0 0 0)

glpi = 3.14 :: GLdouble