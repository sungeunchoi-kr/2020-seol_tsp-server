module TSP where

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Ord
import Data.Sort
import Data.IORef
import System.IO.Unsafe
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)
import Shuffle

import TSPData

-- interesting read:
-- https://arxiv.org/pdf/1808.08329.pdf

-- | Calculates the distance of the gene on the given graph.
geneDist :: Graph -> Gene -> Float
geneDist _ [] = 0
geneDist g xs = geneDist' g (head xs) xs
    where
    geneDist' _ _ [] = 0
    geneDist' _ _ (x:[]) = 0
    geneDist' g s (x:y:[]) = graphDist g x y + (graphDist g y s) -- include route back home.
    geneDist' g s (x:y:rs) = graphDist g x y + (geneDist' g s (y:rs))

bestInGenePool :: Graph -> [Gene] -> Gene
bestInGenePool cities = minimumBy (comparing $ geneDist cities)

worstInGenePool :: Graph -> [Gene] -> Gene
worstInGenePool cities = maximumBy (comparing $ geneDist cities)

createGenePool :: (MonadRandom m) => Int -> [NodeLbl] -> m [[NodeLbl]]
createGenePool n = replicateM n . shuffle

tournamentSelection :: Graph -> Int -> [Gene] -> [Gene]
tournamentSelection _ _ [] = []
tournamentSelection cities n xs = 
    let competitors = take n xs
        xsRemainder = drop n xs
        winner = bestInGenePool cities competitors in 
    winner : (tournamentSelection cities n xsRemainder)

-- order 1 crossover 
-- http://www.rubicite.com/Tutorials/GeneticAlgorithms/CrossoverOperators/Order1CrossoverOperator.aspx
order1Crossover :: (MonadRandom m) => Gene -> Gene -> m Gene
order1Crossover p1 p2 = do
    -- 1. choose a 'swath' from p1. (random length and starting position)
    -- 2. For all elements of p2: Is it a member of the swath?
    --    If so, skip. Otherwise, append.
    -- 3. Inject the swath into p2 at the same position as p1.
    rlen <- getRandomR (1, length p1) 
    rpos <- getRandomR (0, length p1 - rlen)
    let swath = take rlen . drop rpos $ p1  -- sublist
    let swathSet = Set.fromList swath
    let ch = filter (\n -> Set.notMember n swathSet) p2
    return $ (take rpos ch) ++ swath ++ (drop rpos ch)

crossover :: (MonadRandom m) => Int -> [Gene] -> m [Gene]
crossover n gs = do
    let parentPair = foldl (++) [] $ take n $ repeat $ pairup gs
    forM parentPair $ uncurry order1Crossover
    where
        pairup :: [a] -> [(a,a)]
        pairup [] = []
        pairup (x:y:zs) = (x,y) : (pairup zs)
        pairup (x:zs) = (x,x) : (pairup zs)

mutate :: (MonadRandom m) => Float -> Gene -> m Gene
mutate p g = do
    p_i <- getRandom
    if p_i > p
        then return g
        else operation g
    where
        operation :: (MonadRandom m) => Gene -> m Gene
        operation g = do
            i1 <- getRandomR (0, length g - 1)
            i2 <- getRandomR (0, length g - 1)
            return $ swapElementsAt i1 i2 g

        swapElementsAt :: Int -> Int -> [a] -> [a]
        swapElementsAt a b list = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
            where   list1 = take a list;
                    list2 = drop (succ a) (take b list);
                    list3 = drop (succ b) list

mutateN :: (MonadRandom m) => Float -> [Gene] -> m [Gene]
mutateN p gs = forM gs (mutate p)

poolDI :: Graph -> [Gene] -> Float
poolDI graph gene = 
    (med - min)
    where med = medianDistInGenePool graph gene
          min = geneDist graph $ bestInGenePool graph gene
    
generation :: (MonadRandom m) => Graph -> [Gene] -> m [Gene]
generation cities g0 = do
    let di = poolDI cities g0  -- the gene pool diversity index
    if di > 0.0
        then do
            let g1 = tournamentSelection cities 3 g0
            offsprings <- crossover 3 g1
            offsprings_mu <- mutateN 0.1 offsprings
            let d = (length g0) - (length offsprings_mu)
            return $ offsprings_mu ++ (take d g0)
        else do
            offsprings_mu <- mutateN 0.9 g0
            --let d = (length g0) - (length offsprings_mu)
            return $ offsprings_mu  -- ++ (take d g0)
            
medianDistInGenePool :: Graph -> [Gene] -> Float
medianDistInGenePool cities pool = 
    let ls = sort $ fmap (geneDist cities) pool
    in ls !! (length ls `div` 2)

generationN :: (RandomGen g) =>
    Graph
    -> (String -> IO ())
    -> (IORef Bool)
    -> [Gene]
    -> RandT g IO ()
--generationN cities _ 0 g0 = generation cities g0
generationN cities printer stopflag g0 = do
    g1 <- generation cities g0
    stop <- liftIO $ readIORef stopflag
    if stop
        then do return ()
        else do
            liftIO $ printer $
                (show $ geneDist cities $ bestInGenePool cities g1)     ++ ";"
                -- ++ (show $ medianDistInGenePool cities g1)              ++ ";"
                -- ++ (show $ poolDI cities g1)                            ++ ";"
                ++ (show $ geneDist cities $ worstInGenePool cities g1) ++ ";"
                ++ (show $ bestInGenePool cities g1)
            generationN cities printer stopflag g1

start :: (RandomGen g) =>
    Graph
    -> (String -> IO ())
    -> (IORef Bool)
    -> RandT g IO ()
start cities printer stopflag = do
    genes <- createGenePool 1000 (listCities cities)
    generationN cities printer stopflag genes

run :: Graph -> (String -> IO ()) -> (IORef Bool) -> IO ()
run g printer stopflag = do
    seed <- (round . (* 1000)) <$> getPOSIXTime 
    genePool <- evalRandT (start g printer stopflag) (mkStdGen seed)
    return ()

