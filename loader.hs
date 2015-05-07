module Loader where

import Graphics.Rendering.OpenGL as GL
import DataStructures as DS
import System.IO
import Data.Maybe
import Numeric.Units.Dimensional.Prelude as D
import Numeric.Units.Dimensional.NonSI
import Data.List (partition)
import PlanetStuff

--type abstractPlanet = (String, Double, Double, Double, String)

load filePath = do
	content <- readFile filePath
	let contentAsLines = lines content
	let list = (map fromJust . filter (isJust) . map makeAbstractObj $ contentAsLines)
	let absCentralObj = fromJust $ findCentralObj list
	let centralObj = centralFromAbstractObj absCentralObj
	let planets = toPlanets ([centralObj],(filter (\x -> x /= absCentralObj) list),[centralObj])
	
	return (dropPlanetNames planets)


makeAbstractObj line = if cond then Just (name, fromJust r, fromJust semiMajor, fromJust mass, parent) else Nothing
	where 	cond = (length ls == 5) && (r /= Nothing) && (semiMajor /= Nothing) && (mass /= Nothing)
		name = head ls
		r = f $ ls 
		semiMajor = f . tail $ ls
		mass = f . tail . tail $ ls
		parent = head . tail . tail . tail . tail $ ls
		ls = words line
		f = getRead . reads . head . tail :: [String] -> Maybe GLdouble
	--format: name r v mass parent

dropPlanetNames [] = []
dropPlanetNames ((a,b):xs) = b : dropPlanetNames xs

toPlanets (_,[],done) = done
toPlanets (cur,todo,done) = toPlanets (newlyDone ++ tail cur, later, done ++ newlyDone)
		where 	this@(n,p) = head cur
			(now,later) = partition (\(_,_,_,_,par) -> par == n) todo
			newlyDone = map (flip toPlanet this) now

toPlanet absObj@(n,r,s,m,_) (_,parent) = (n,makePlanet (r *~ astronomicalUnit) (s *~ astronomicalUnit) (m *~ kilo gram) (getMass parent) `orbits` parent)

centralFromAbstractObj absObj@(n,_,_,m,_) = (n,makeCentralObject (m *~ kilo gram))

findCentralObj [] = Nothing
findCentralObj absObjs@(x:xs) 
	| x == (a,0,0,m,a) = Just x
	| otherwise = findCentralObj xs
	where (a,m) = (\(a,_,_,m,_) -> (a,m)) x


getRead :: [(a,String)] -> Maybe a
getRead [] = Nothing
getRead a = Just b
	where (b,_) = head a


loadConfig filePath = do
	content <- readFile filePath
	let contentAsLines = lines content
	stepSize <- getStepSize contentAsLines
	stepsPerFrame <- getStepsPerFrame contentAsLines
	zoomLevel <- getZoomLevel contentAsLines
	return (stepSize, stepsPerFrame, zoomLevel)

getStepSize ls = do
	value <- (getThingWithDefault ls "stepSize" 2000)
	return (value *~ second)

getStepsPerFrame ls = getThingWithDefault ls "stepsPerFrame" 40

getZoomLevel ls = getThingWithDefault ls "zoomLevel" (0 Prelude.- 30)

getThingWithDefault ls keyword def
	| isJust x = return (fromJust x)
	| otherwise = print (keyword++" not found, using default value") >> return def
	where x = getThing ls keyword

getThing ls keyword = if (length ls > 0 && valid) then (Just num) else Nothing
	where 	rem = dropWhile (\x -> head (words x) /= keyword) ls
		valid = length rem > 0
		num = read . head . tail . words . head $ rem