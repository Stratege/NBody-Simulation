{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module BodyGravityForces where

import Physics
import Numeric.Units.Dimensional.Prelude as D hiding (concat, foldl)
import Numeric.Units.Dimensional.NonSI
import qualified Prelude hiding (concat, foldl)
import Graphics.Rendering.OpenGL as GL
import DataStructures as DS
import Data.List as List hiding (concat, foldl)
import qualified Data.Sequence as Seq
import Data.Foldable
--import Data.Functor


--optimized
optimizedCalculateAccelerationsFromGravity objects = Seq.zipWith (addForceAsAcceleration) objects (forceVectors)
	where 	forceVectors = optSumForceVectorsByObjectID . optGetTotalGravityForces $ (objectsWithIndex)
		objectsWithIndex = Seq.zipWith (\(!x) (!y) -> (x,y)) (Seq.fromList [1..(Seq.length objects)]) objects

optGetGravityForceVector :: Mass GLdouble -> Mass GLdouble -> Vector3 (D.Length GLdouble) -> Vector3 (D.Length GLdouble) -> (Vector3 (Force GLdouble) , Vector3 (Force GLdouble))
optGetGravityForceVector m1 m2 p1 p2 = (vectorTimesScalar normalizedDVec (negate force), vectorTimesScalar normalizedDVec force)
			where 	force = bigG * m1 * m2 / (vecMag * vecMag)
				vecMag = vectorMagnitude dVec
				normalizedDVec = vectorNormalize dVec
				dVec = vectorSubstract p1 p2

optSumForceVectorsByObjectID (Seq.viewl -> Seq.EmptyL) = Seq.empty
optSumForceVectorsByObjectID (Seq.viewl -> (i,vec) Seq.:< xs) = foldl (\acc (j,x) -> vectorAdd acc x) vec sameObj Seq.<| optSumForceVectorsByObjectID remlist
		where 	sameObj = Seq.takeWhileL (\(j,_) -> j == i) xs
			remlist = Seq.drop (Seq.length sameObj) xs



optGetTotalGravityForces objects = Seq.sortBy (sortObj) . asum . optFoo (f) $ objects
	where 	f (i,(Object p1 _ _ _ m1)) xs = asum . fmap (g i p1 m1) $ xs
		g i p1 m1 = (\(j,(Object p2 _ _ _ m2)) -> let (!f1,!f2) = optGetGravityForceVector m1 m2 p1 p2 in ((i,f1) Seq.<| (j,f2) Seq.<| Seq.empty ))


optFoo _ (Seq.viewl -> Seq.EmptyL) = Seq.empty
optFoo f (Seq.viewl -> x Seq.:< xs) = f x xs Seq.<| optFoo f xs














--non optimized

calculateAccelerationsFromGravity objects = zipWith (addForceAsAcceleration) objects forceVectors
	where 	forceVectors = sumForceVectorsByObjectID . getTotalGravityForces $ objectsWithIndex
		objectsWithIndex = zipWith (\x y -> (x,y)) [1..] objects


--acceleration calculation, first: 2 bodies
getGravityForceVector :: Mass GLdouble -> Mass GLdouble -> Vector3 (D.Length GLdouble) -> Vector3 (D.Length GLdouble) -> (Vector3 (Force GLdouble) , Vector3 (Force GLdouble))
getGravityForceVector m1 m2 p1 p2 = (vectorTimesScalar normalizedDVec (negate force), vectorTimesScalar normalizedDVec force)
			where 	force = bigG * m1 * m2 / (vecMag * vecMag)
				vecMag = vectorMagnitude dVec
				normalizedDVec = vectorNormalize dVec
				dVec = vectorSubstract p1 p2

sumForceVectorsByObjectID [] = []
sumForceVectorsByObjectID ((i,vec):xs) = foldl (\acc (j,x) -> vectorAdd acc x) vec sameObj : sumForceVectorsByObjectID remlist
		where 	sameObj = takeWhile (\(j,_) -> j == i) xs
			remlist = List.drop (List.length sameObj) xs



getTotalGravityForces objects = sortBy (sortObj) . concat . foo' (f) $! objects
	where 	f (i,(Object p1 _ _ _ m1)) xs = concat . map (g i p1 m1) $ xs
		g i p1 m1 = (\(j,(Object p2 _ _ _ m2)) -> let (f1,f2) = getGravityForceVector m1 m2 p1 p2 in [(i,f1),(j,f2)])


foo _ [] = [[]]
foo f (x:xs) = f x xs : foo f xs

foo' f (x:xs) = (f x xs) : (foo f xs)






--shared


--sortObj :: Num a => (a,Vector3 b) -> (a,Vector3 b) -> Ordering
sortObj (i,_) (j,_)
	| i < j = LT
	| i > j = GT
	| i == j = EQ




addAcceleration (Object p o v a m) aVec = (Object p o v (vectorAdd a aVec) m)
addForceAsAcceleration obj@(Object _ _ _ _ m) fVec = addAcceleration obj (vectorTimesScalar fVec ((num 1)/m))
