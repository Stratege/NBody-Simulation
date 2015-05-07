{-# LANGUAGE BangPatterns #-}
module Integrators where

import Physics
import Numeric.Units.Dimensional.Prelude as D
import Numeric.Units.Dimensional.NonSI
import qualified Prelude
import Graphics.Rendering.OpenGL as GL
import BodyGravityForces
import DataStructures as DS
import Data.List as List
import Data.Sequence as Seq

stepsize = 2000 *~ second

--foo :: (a -> b -> a) -> a -> b -> a
--foo f y t = y
--	where	y_next = y + stepsize/6 * (k1 + 2*k2 + 2*k3 + k4)
--		t_next = t + stepsize
--		k1 = f t y
--		k2 = f (t + stepsize / 2) (y + stepsize/2 * k1)
--		k3 = f (t + stepsize / 2) (y + stepsize/2 * k2)
--		k4 = f (t + stepsize) (y + stepsize * k3)

--bar t ((Vector3 px, py, pz), (Vector3 vx, vy, vz))

-- http://spiff.rit.edu/richmond/nbody/OrbitRungeKutta4.pdf yay

--vi_next ri = a*ri

--alternatingly:
--update acceleration
--do runge kutta step
--or in other words:
-- ???


--symplectic 

useVelocityVerlet objects = objects `seq` (flip (velocityVerlet calculateAccelerationsFromGravity) stepsize $! accClearedObjs)
		where 	accClearedObjs = fmap (\(Object p o v a m) -> (Object p o v (Vector3 zeroAcc zeroAcc zeroAcc) m)) objects
			zeroAcc = (0 *~ (meter / second / second))

optUseVelocityVerlet objects = objects `seq` (flip (velocityVerlet optimizedCalculateAccelerationsFromGravity) stepsize $! accClearedObjs)
		where 	accClearedObjs = fmap (\(Object p o v a m) -> (Object p o v (Vector3 zeroAcc zeroAcc zeroAcc) m)) objects
			zeroAcc = (0 *~ (meter / second / second))

velocityVerlet accFunc objects t = objsVel1
		where	objsVel0_5 = fmap (\(Object p o v a m) -> (Object p o (vel0_5 v a) a m)) objects
			vel0_5 vel0 acc0 = vectorAdd vel0 (vectorTimesScalar acc0 ((num 0.5) * t))
			objsPos1 = fmap (\(Object p o v a m) -> (Object (pos1 p v) o v a m)) objsVel0_5
			pos1 pos0 vel0_5 = vectorAdd pos0 (vectorTimesScalar vel0_5 t)
			objsAcc1 = accFunc objsPos1 -- actually: derive acc1 from the interaction potential using x1
			objsVel1 = fmap (\(Object p o v a m) -> (Object p o (vel1 v a) a m)) objsAcc1
			vel1 vel0_5 acc1  = vectorAdd vel0_5 (vectorTimesScalar acc1 ((num 0.5) * t))

useExplicitEuler = flip (explicitEuler optimizedCalculateAccelerationsFromGravity) stepsize

explicitEuler accFunc objects t = fmap (\(Object p o v a m) -> (Object (pos1 v) o (vel1 a) a m)) objAcc0
	where 	pos1 vel0 = vectorTimesScalar vel0 t
		vel1 acc0 = vectorTimesScalar acc0 t
		objAcc0 = accFunc objects

useStroemerMethod :: Time GLdouble -> Seq.Seq Object -> Seq.Seq Object
useStroemerMethod = flip (stroemerMethod optimizedCalculateAccelerationsFromGravity)

stroemerMethod accFunc objects t = objsPos
	where 	objsPos = fmap (\(Object !p !o !v !a !m) -> (Object (pos_new p (vectorTimesScalar v  (1 *~ second)) a) o (vectorTimesScalar p  ((num 1) / (1 *~ second))) nullAccVec m)) objsAccNew
		pos_new p p_old a = (vectorAdd (vectorSubstract (vectorTimesScalar p (num 2)) p_old) (vectorTimesScalar a (t * t)))
		objsAccNew = accFunc objects
		nullAccVec = (Vector3 nullAcc nullAcc nullAcc)
		nullAcc = 0 *~ (meter / second / second)

--a1 = a1 + a1inc
--a2 = a2 + a2inc
--(a1inc,a2inc) = (\(a,b) -> ((acc m a),(acc m b))) $ getGravityForceVector m1 m2 p1 p2

--main = print helper
--helper = (foldl (\acc f -> useVelocityVerlet acc) objMaker $ [1..864000])