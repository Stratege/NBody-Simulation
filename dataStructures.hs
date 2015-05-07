{-# LANGUAGE BangPatterns #-}
module DataStructures (module DataStructures, num) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Numeric.Units.Dimensional.Prelude as D
import Numeric.Units.Dimensional.NonSI
import qualified Prelude
import Physics

data Keyboard = Keyboard {
keya :: KeyButtonState,
keyb :: KeyButtonState,
keyc :: KeyButtonState,
keyd :: KeyButtonState,
keye :: KeyButtonState,
keyf :: KeyButtonState,
keyg :: KeyButtonState,
keyh :: KeyButtonState,
keyi :: KeyButtonState,
keyj :: KeyButtonState,
keyk :: KeyButtonState,
keyl :: KeyButtonState,
keym :: KeyButtonState,
keyn :: KeyButtonState,
keyo :: KeyButtonState,
keyp :: KeyButtonState,
keyq :: KeyButtonState,
keyr :: KeyButtonState,
keys :: KeyButtonState,
keyt :: KeyButtonState,
keyu :: KeyButtonState,
keyv :: KeyButtonState,
keyw :: KeyButtonState,
keyx :: KeyButtonState,
keyy :: KeyButtonState,
keyz :: KeyButtonState,
key0 :: KeyButtonState,
key1 :: KeyButtonState,
key2 :: KeyButtonState,
key3 :: KeyButtonState,
key4 :: KeyButtonState,
key5 :: KeyButtonState,
key6 :: KeyButtonState,
key7 :: KeyButtonState,
key8 :: KeyButtonState,
key9 :: KeyButtonState
} deriving (Show)

data Mouse = Mouse {
 pos :: Position,
 leftButton :: KeyButtonState,
 rightButton :: KeyButtonState
} deriving (Show)


type DataMass = D.Mass GLdouble
type DataLength = D.Length GLdouble
type DataVelocity = D.Velocity GLdouble
type DataAcceleration = D.Acceleration GLdouble


type ObjPosition = Vector3 (D.Length GLdouble)
type ObjOrientation = Vector3 (Dimensionless GLdouble)
type ObjVelocity = Vector3 (Velocity GLdouble)
type ObjAcceleration = Vector3 (Acceleration GLdouble)
type ObjMass = Mass GLdouble
type ObjRadius = D.Length GLdouble
type Point = Vector3 GLdouble

au :: Num a => Unit DLength a
au = astronomicalUnit

data Object = Object !ObjPosition !ObjOrientation !ObjVelocity !ObjAcceleration !ObjMass deriving Show
data Sphere = Sphere !Object !ObjRadius deriving Show

standardSphere = (DataStructures.Sphere standardObject ((num 10) * m))
		where   m = 1 *~ meter
standardObject = (Object (Vector3 m m m) (Vector3 o o o) (Vector3 v v v) (Vector3 a a a) (1 *~ kilo gram))
		where   m = 1 *~ meter
			v = 0 *~ (meter / second)
			a = 0 *~ (meter / second / second)
			o = 1 *~ one


deMeterize a = a /~ meter
vectorDeMeterize (Vector3 a b c) = (Vector3 (deMeterize a) (deMeterize b) (deMeterize c))


vectorSubstract (Vector3 v1x v1y v1z) (Vector3 v2x v2y v2z) = (Vector3 (v1x-v2x) (v1y-v2y) (v1z-v2z))
vectorAdd (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (Vector3 (x1+x2) (y1+y2) (z1+z2))

vectorNormalize v = vectorTimesScalar v ((num 1) / (vectorMagnitude v))
vectorTimesScalar (Vector3 x y z) s = (Vector3 (x*s) (y*s) (z*s))

--this function is a problem, in terms of performance
vectorMagnitude (Vector3 vx vy vz) = sqrt (vx * vx + vy * vy + vz * vz)
vectorSqrtMagnitude (Vector3 vx vy vz) = vx * vx + vy * vy + vz * vz
vectorNormalizeDivMagniutde v = vectorTimesScalar v ((num 1) / (vectorSqrtMagnitude v))


getPos (Object p _ _ _ _) = p

deMass a = a /~ (kilo gram)

