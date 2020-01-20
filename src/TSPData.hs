module TSPData where

import Control.Applicative
import qualified Data.Map as Map

type Position = [Float]

type GrpahDist = Float
type NodeLbl = Int
type Graph = Map.Map NodeLbl Position
type Gene = [NodeLbl]
data TravState = TravState [NodeLbl] (Map.Map NodeLbl GrpahDist)

listCities :: Graph -> [NodeLbl]
listCities = Map.keys

graphDist :: Graph -> NodeLbl -> NodeLbl -> Float
graphDist g n1 n2 =
    distance p1 p2
    where
        p1 = g Map.! n1
        p2 = g Map.! n2
        --square = flip (**) 2
        --deltasq = square <$> zipWith (-) p1 p2

distance :: Floating n => [n] -> [n] -> n
distance p1 p2 = sqrt $ foldl squareAndSum 0 $ zipWith (-) p1 p2
    where
        squareAndSum acc e = acc + (e ** 2)

loadCitiesDataAsGraph :: [String] -> TSPData.Graph
loadCitiesDataAsGraph lines = 
    Map.fromList $ parseline <$> lines
    where
        parseline :: String -> (NodeLbl, Position)
        parseline line = toTuple $ words line

        toTuple :: [String] -> (NodeLbl, Position)
        toTuple (n:ps) = ( (read n), fmap read ps )
        toTuple _ = ( -1, [] )

load:: String -> TSPData.Graph
load dat = 
    Map.fromList $ parseline <$> (lines dat)
    where
        parseline :: String -> (NodeLbl, Position)
        parseline line = toTuple $ words line

        toTuple :: [String] -> (NodeLbl, Position)
        toTuple (n:ps) = ( (read n), fmap read ps )
        toTuple _ = ( -1, [] )
