module TSPData where

import qualified Data.IntMap.Strict as Map

type Position = [Float]

type GrpahDist = Float
type NodeLbl = Int
type Graph = Map.IntMap Position
type Gene = [NodeLbl]
data TravState = TravState [NodeLbl] (Map.IntMap GrpahDist)

listCities :: Graph -> [NodeLbl]
listCities = Map.keys

load:: String -> TSPData.Graph
load dat = 
    Map.fromList $ parseline <$> lines dat
    where
        parseline :: String -> (NodeLbl, Position)
        parseline line = toTuple $ words line

        toTuple :: [String] -> (NodeLbl, Position)
        toTuple (n:ps) = ( (read n), fmap read ps )
        toTuple _ = ( -1, [] )
