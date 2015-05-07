module Physics where

-- this gives us all we need to rule supreme. Let's build things with it!
import Numeric.Units.Dimensional.Prelude as D
import Numeric.Units.Dimensional.NonSI
import qualified Prelude
import Graphics.Rendering.OpenGL as GL

--quick extensions to make Dimensional more useable

toQuantity :: (Num a) => Unit d a -> Quantity d a
toQuantity x = 1 *~ x

num x = (x *~ one)

density :: Fractional a => Mass a -> Volume a -> MassDensity a
density mass volume = mass / volume


--SPACE - The final frontier!

bigG = (6.6738480e-11 *~ (meter * meter * meter / (kilo gram) / second / second))

gravity :: Mass GLdouble -> Mass GLdouble -> D.Length GLdouble -> Force GLdouble
gravity m1 m2 dist = bigG * m1 * m2 / (dist * dist)

rocketThrust :: Num a => MassFlow a -> Velocity a -> Force a
rocketThrust massFlowRate exhaustVelocity = massFlowRate * exhaustVelocity

--massFlowRate :: Num a => 

tsiolkovskyEquation :: (Floating a, Fractional a) => Velocity a -> Mass a -> Mass a -> Velocity a
tsiolkovskyEquation ve m0 m1 = ve * log (m0 / m1)

step mfr ve mship mearth dist vel tick = vel + dv
	where dv = tsiolkovskyEquation ve mship mFin - (gravity mship mearth dist) * tick
	      mFin = mship - mfr * tick

--IF I were to model a waterfall...
--I'd need:
--behaviour of water in a fall
--behaviour of water when accelerated somewhere during the fall
--fluiddynamics, basically


--http://en.wikipedia.org/wiki/Continuity_equation
--http://en.wikipedia.org/wiki/Cross_section_(geometry)
--http://en.wikipedia.org/wiki/Vector_area
--http://en.wikipedia.org/w/index.php?title=Special%3ABookSources&isbn=9780071487818#Europe
--http://en.wikipedia.org/wiki/Specific_impulse#Specific_impulse_as_a_speed_.28effective_exhaust_velocity.29
--http://en.wikipedia.org/wiki/Fluid_dynamics
--http://en.wikipedia.org/wiki/Mass_flow_rate#cite_note-1
--http://de.wikipedia.org/wiki/Gravitationskonstante
--http://www.engineeringtoolbox.com/fluid-density-temperature-pressure-d_309.html
--https://hackage.haskell.org/package/dimensional-0.13.0.1/docs/Numeric-Units-Dimensional-Quantities.html
--http://en.wikipedia.org/wiki/Properties_of_water#Density_of_water_and_ice
--http://en.wikipedia.org/wiki/Density
--https://www.google.de/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=waterfall+physics
--http://roll20.net/
--http://www.srh-cube.de/cube-konzept/cube-market/



--from class: Auftriebskraft (buouancy or some spelling like that)
buouancy :: Num a => MassDensity a -> Acceleration a -> Volume a -> Force a
buouancy densityMedium gravity vol = densityMedium * gravity * vol 

densityWater = 1000 *~ (kilo gram / ( meter * meter * meter)) -- note: this is a fairly crude approximation

buouancyWater = buouancy densityWater

gravityForce :: Num a => Acceleration a -> Mass a -> Force a
gravityForce gravity mass = gravity * mass

buouancyFactor :: Fractional a => MassDensity a -> MassDensity a -> Dimensionless a
buouancyFactor densityMedium densityObject= (densityMedium / densityObject) 

linearFlowResistance :: Num a => DynamicViscosity a -> Velocity a -> D.Length a -> Force a
linearFlowResistance viscosity vel characteristicLength = (viscosity * vel * characteristicLength)

massFromVol :: Num a => MassDensity a -> Volume a -> Mass a
massFromVol densityObject vol = densityObject * vol



--collision of point masses (fully elastic)
--ma * va + mb * vb = ma (va') + mb * (vb')
--1/2 * ma * (va)^2 + 1/2 * mb * (vb)^2 = 1/2 * ma * (va')^2 + 1/2 * mb * (vb')^2
--1. ma * (va-va') = mb * (vb - vb')
--2. ma * (va)^2 + mb * (vb)^2 = ma * (va')^2 + mb * (vb')^2
--3. ma * ((va)^2 - (va')^2) = mb * ((vb')^2 - (vb)^2)
--4. ma * (va + va') * (va - va') = mb * (vb' + vb) * (vb' - vb)
--now we see that 1. is in 4. and can thereby get to:
--5. va + va' = vb + vb'
--6. va' - vb' = vb - va
--7. va' - vb' = - (va - vb)
--thusly we know that the difference in velocity before and after collision are the same but in opposite direction
-- dv = va' - vb' = - ( va - vb)
-- dv = r * (1/ma) + r * (1/ma)
-- dv = r * (1/ma + 1/mb)
-- r = dv / (1/ma + 1/mb)
-- r is the impulse
-- Pa' = Pa_deriv + 1/ma * r
-- Pb' = Pb_deriv + 1/mb * r

impulse :: Num a => Mass a -> Velocity a -> Impulse a
impulse m v = m * v

--still collision of point masses (fully elastic) - now 3d!
--Kontaktnormale (normal between the two points)
--n_vec = (Pa_vec - Pb_vec) * 1 / (|pa_vec - pb_vec|)
--vs = (Pb_deriv - Pa_deriv) * n_vec
--elastic collision: vs' = -vs
--Restitutionskoeffizient c : vs' = - c * vs
--vectorial: 	Pa'_deriv_vec = Pa_deriv_vec + 1 / ma * (vs * (1-c)) / (1/ma + 1/mb) * n_vec
--		Pb'_deriv_vec = Pb_deriv_vec + 1 / mb * (vs * (1-c)) / (1/ma + 1/mb) * n_vec

--Objekte dringen ineinander ein
--Ausflösung der Eindringung
--Eindringungstiefe d (kreise/kügeln)
-- pa_delta + pb_delta = d
-- ma*pa_delta = mb*pb_delta
-- pb_delta = ma/mb * pa_delta
-- pa_delta + ma/mb * pb_delta = d
-- pa_delta * (1 + ma/mb) = d
-- pa_delta * ((mb + ma) / mb) = d
-- pa_delta = mb/(mb+ma) * d





--runge kutta stuff
--euler method: lim(t_delta -> 0)( (y(t0 + t_delta) - y(t0)) / t_delta) = f(t0,y0)
-- y1 = y0 + f(t0,y0) * t_delta
-- where y(t0) = y0