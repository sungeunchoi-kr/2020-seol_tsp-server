module TSPMain where

import System.Environment
import System.Exit
import Data.IORef
import qualified Data.Map as Map
import qualified TSP as TSP
import qualified TSPData as TSPData

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = usage   >> exit
parse [s]    = do
    dat <- readFile s
    let g = TSPData.loadCitiesDataAsGraph $ lines dat

    stopflag <- newIORef False
    TSP.run g putStrLn stopflag 

usage   = putStrLn "Usage: [-vh] [file ..]"
version = putStrLn "0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

main :: IO ()
main = do
    args <- getArgs
    parse args

