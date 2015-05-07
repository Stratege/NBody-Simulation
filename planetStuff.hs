module PlanetStuff where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import DataStructures as DS
import Physics
import Numeric.Units.Dimensional.Prelude as D
import Numeric.Units.Dimensional.NonSI
import qualified Prelude
import qualified Data.Sequence as Seq

objMaker = Seq.fromList [sun,mercury,earth,moon,mars,jupiter,pluto,charon,bigUFO] -- map (\x -> let (Object p o v a m) = standardObject in (Object (vectorTimesScalar p (num (100 Prelude.* x))) o (vectorTimesScalar v (num (100 Prelude.* x))) a (m * (num 1000000000)))) [1..2]

--sun is in the middle of our frame, at first atleast
--sun = makePlanet nullMeter v (1.989e30 *~ kilo gram)
sun = makeCentralObject sunMass
sunMass = (1.989e30 *~ kilo gram)

--earth = makePlanet (1.5e11 *~ meter) (29.77 *~ (kilo meter / second)) (5.974e24 *~ kilo gram)
earth = makePlanet distMax semiMajor earthMass sunMass
	where   distMax = 1.017 *~ astronomicalUnit
		semiMajor = 1 *~ astronomicalUnit
earthMass = (5.974e24 *~ kilo gram)

--moon = makePlanet (384e6 *~ meter) (1 *~ (kilo meter / second)) (7.349e22 *~ kilo gram) `moonOf` earth
moon = makePlanet (405.5e6 *~ meter) (384.4e6 *~ meter) (7.349e22 *~ kilo gram) earthMass `orbits` earth

--mars = makePlanet (2.28e11 *~ meter) (24.13 *~ (kilo meter / second)) (6.419e23 *~ kilo gram)
mars = makePlanet (1.666 *~ astronomicalUnit) (1.524 *~ astronomicalUnit) (6.419e23 *~ kilo gram) sunMass

--mercury = makePlanet (5.79e10 *~ meter) (47.36 *~ (kilo meter / second)) (3.301e23 *~ kilo gram)
mercury = makePlanet (0.467 *~ astronomicalUnit) (0.3871 *~ astronomicalUnit) (3.301e23 *~ kilo gram) sunMass 

--jupiter = makePlanet (7.78e11 *~ meter) (13.07 *~ (kilo meter / second)) (1.899e27 *~ kilo gram)
jupiter = makePlanet (5.46 *~ astronomicalUnit) (5.203 *~ astronomicalUnit) (1.899e27 *~ kilo gram) sunMass 

--pluto = makePlanet (5.906e12 *~ meter) (4.72 *~ (kilo meter / second)) (1.25e22 *~ kilo gram)
pluto = makePlanet (49.305 *~ astronomicalUnit) (39.482 *~ astronomicalUnit) plutoMass sunMass 
plutoMass = (1.25e22 *~ kilo gram)

--charon is not going to work, probably, but oh well
charon = makePlanet (19752.8 *~ kilo meter) (19571.4 *~ kilo meter) charonMass plutoMass `orbits` pluto
charonMass = (1.52e21 *~ kilo gram)

bigUFO = makePlanet (60000 *~ kilo meter) (60000 *~ kilo meter) (1.52e20 *~ kilo gram) (plutoMass + charonMass) `orbits` pluto

makePlanet distFromSun semiMajor mass pMass = (Object (Vector3 distFromSun nullMeter nullMeter) (Vector3 o o o) (Vector3 v velAtPos v) (Vector3 a a a) mass)
	where   velAtPos = sqrt (gm * foo)
		foo = (num 2) / distFromSun - (num 1) / semiMajor
		gm = bigG * pMass

moon@(Object p1 o v1 a m) `orbits` planet@(Object p2 _ v2 _ _) = (Object (vectorAdd p1 p2) o (vectorAdd v1 v2) a m)

getMass planet@(Object _ _ _ _ m) = m

makeCentralObject mass = (Object (Vector3 nullMeter nullMeter nullMeter) (Vector3 o o o) (Vector3 v v v) (Vector3 a a a) mass)

nullMeter = 0 *~ meter
o = 1 *~ one
v = 0 *~ (meter / second)
a = 0 *~ (meter / second / second)

objMakerStroemer stepSize = fmap (toStroemer stepSize) objMaker

--sort of a hack, writing the old position into the velocity variable but this allows the use of same datatype for stroemer method
toStroemer stepSize (Object p o v a m) = (Object p o (vectorTimesScalar (vectorSubstract p (vectorTimesScalar v stepSize)) (num 1 / (1 *~ second))) a m)