{-# LANGUAGE BangPatterns #-}
module WSC where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import DataStructures as DS
import Physics
import Numeric.Units.Dimensional.Prelude as D
import Numeric.Units.Dimensional.NonSI
import qualified Prelude
import Integrators
import qualified Data.Sequence as Seq
import Loader (load,loadConfig)
import PlanetStuff

--loadConfig a = return ((2000 :: GLdouble) *~ second, 40 :: Int, -30 :: Int)

--Stroemer
loadStartState = do
	planets <- load "planets.txt"
	(stepSize,stepsPerFrame,zoomLevel) <- loadConfig "config.txt"
	return (stepSize,(1 :: Int,zoomLevel :: Int, Seq.fromList (fmap (toStroemer stepSize) planets), (Vector3 (0 *~ meter :: D.Length GLdouble) (0 *~ meter) (0 *~ meter)), (freeKeyboard,stepsPerFrame :: Int)))

freeKeyboard = Keyboard GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release GLFW.Release

wsc :: (Seq.Seq Object -> Seq.Seq Object) -> (Int, Int, Seq.Seq Object, ObjPosition, (Keyboard,Int)) -> (Keyboard,Mouse) -> (Int, Int, Seq.Seq Object, ObjPosition, (Keyboard,Int))
wsc method oldState@(viewObjNumOld,zoomLevelOld,prevObjects, oldCam, (prevKB,prevStepsPerFrame)) inputs@(keyboard,mouse) = (viewObjNumNew,zoomLevelNew, objects, newCam, (keyboard, newStepsPerFrame))
	where   viewObjNumNew = (clampPlanetNum prevObjects) $ changeNum keyboard prevKB viewObjNumOld
		zoomLevelNew = changeZoomLevel keyboard prevKB zoomLevelOld
 		objects = foldl (\(!x) y -> method x) prevObjects [1..prevStepsPerFrame]
		newCam = cameraChange oldCam keyboard
		-- method = useStroemerMethod -- optUseVelocityVerlet -- useExplicitEuler
		newStepsPerFrame = changeSteps keyboard prevKB prevStepsPerFrame

--wsc :: (KeyButtonState, [Vector3 GLdouble], WSC.Sphere, ObjPosition) -> (Keyboard,Mouse) -> (KeyButtonState, [Vector3 GLdouble], DS.Sphere, ObjPosition)
--wsc oldState@(n,prevLines,prevCircle, oldCam) inputs@(keyboard,mouse) = (m, lines,circle, newCam)
--	where   (m,lines) = lineChange n prevLines (mCameraToWorld oldCam mouse)
-- 		circle = circleFall . (\(WSC.Sphere obj r) -> (WSC.Sphere (objectCollision obj (num 0.9)) r)) $ prevCircle
--		newCam = cameraChange oldCam keyboard

changeSteps kb prevKB old
	| justPressedKey keye kb prevKB = old Prelude.* 2
	| old == 1 = old
	| justPressedKey keyd kb prevKB = floor ((fromIntegral old) Prelude./ 2)
	| otherwise = old

changeNum kb prevKB old
	| justPressedKey key1 kb prevKB = 1
	| justPressedKey key2 kb prevKB = 2
	| justPressedKey key3 kb prevKB = 3
	| justPressedKey key4 kb prevKB = 4
	| justPressedKey key5 kb prevKB = 5
	| justPressedKey key6 kb prevKB = 6
	| justPressedKey key7 kb prevKB = 7
	| otherwise = changePlanetNum kb prevKB old

changePlanetNum kb prevKB old
	| justPressedKey keyt kb prevKB = old Prelude.+ 1
	| justPressedKey keyg kb prevKB = old Prelude.- 1
	| otherwise = old

clampPlanetNum planets num
	| num < 1 = 1
	| num > length = length
	| otherwise = num
	where length = Seq.length planets

changeZoomLevel kb prevKB oldZoom
	| justPressedKey keyr kb prevKB = oldZoom Prelude.+ 1
	| justPressedKey keyf kb prevKB = oldZoom Prelude.- 1
	| otherwise = oldZoom


justReleasedKey keyFunc kb prevKB = keyFunc kb == GLFW.Release && keyFunc prevKB == GLFW.Press

justPressedKey keyFunc kb prevKB = keyFunc kb == GLFW.Press && keyFunc prevKB == GLFW.Release

circleFall (DS.Sphere object r) = let t = (0.01 *~ second) in DS.Sphere (objectFall object t) r

objectFall (Object p@(Vector3 x y z) orientation v@(Vector3 vx vy vz) a@(Vector3 ax ay az) m) t = 
	(Object (Vector3 (x+(vx*t)) (y+(vy*t)) (z+(vz*t))) orientation
		(Vector3 (vx+(ax*t)) (vy+((ay+g)*t)) (vz+(az*t)))
		a m)
	where g = negate (9.81 *~ (meter / second / second))

objectCollision obj@(Object p@(Vector3 x y z) ori v@(Vector3 vx vy vz) a@(Vector3 ax ay az) m) c
	| y < ((-200) *~ meter) && abs(vy) < abs(ay * (1 *~ second)) = (Object p ori (Vector3 vx (0 *~ (meter/second)) vz) (Vector3 nullA nullA nullA) m)
	| y < ((-200) *~ meter) && vy < (0 *~ (meter / second)) = (Object p ori (Vector3 vx (negate (vy * c)) vz) (Vector3 nullA nullA nullA) m)
	| otherwise = obj
	where nullA = (0 *~ (meter / second / second))

lineChange :: KeyButtonState -> [Vector3 GLdouble] -> Mouse -> (KeyButtonState, [Vector3 GLdouble])
lineChange n prevLines mouse
 | n == GLFW.Release = waitForPress prevLines mouse
 | n == GLFW.Press   = waitForRelease prevLines mouse
	

cameraChange :: ObjPosition -> Keyboard -> ObjPosition
cameraChange oldCam@(Vector3 x y z) keyboard = Vector3 (x+a+d) (y+w+s) (z+q+e)
  where a = f (keya)
  	d = negate . f $ (keyd)
  	w = negate . f $ (keyw) 
  	s = f (keys)
	q = f (keyq)
	e = negate . f $ (keye)
  	f a = if (isPressed . a $ keyboard) then (10 *~ meter) else (0 *~ meter)

waitForPress prevLines (Mouse pos@(GL.Position x y) l r) = 
      case l of
        GLFW.Release -> (GLFW.Release, prevLines)
        GLFW.Press   -> let lines = (newLinePoint:newLinePoint :prevLines) in
		          (GLFW.Press, lines)
	where newLinePoint = (Vector3 (fromIntegral x) (fromIntegral y) 0)

waitForRelease prevLines (Mouse pos@(GL.Position x y) l r) = let lines = ((((newLinePoint):) . tail) prevLines) in (l, lines)
	where newLinePoint = (Vector3 (fromIntegral x) (fromIntegral y) 0)

isPressed GLFW.Press = True
isPressed GLFW.Release = False

mCameraToWorld :: Vector3 (D.Length GLdouble) -> Mouse -> Mouse
mCameraToWorld oldCam (Mouse pos l r) = (Mouse (cameraToWorld oldCam pos) l r)

cameraToWorld oldCam@(Vector3 a b c) (GL.Position x y) = (GL.Position (x Prelude.- truncate (a /~ meter)) (y Prelude.- truncate (b /~ meter)))


