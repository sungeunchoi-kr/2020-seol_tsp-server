module TSP where

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Identity
import qualified Data.IntMap.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Ord
import Data.Sort
import Data.IORef
import System.IO.Unsafe
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)
import Shuffle
import Debug.Trace

import TSPData

-- | Calculates the "total points earned" of the gene on the given graph.
geneDist :: Graph -> Gene -> Float
geneDist _ [] = 0
geneDist g ns@(a:ns') = sum $ zipWith cost2 ns (ns'++[a])
    where
        cost2 n1 n2 = 
            let [ lat1, lon1, _  ] = g Map.! n1
                [ lat2, lon2, a2 ] = g Map.! n2 in
            a2 - ( gyotongbi $ latlonDist_km (lat1,lon1) (lat2,lon2) )

        gyotongbi km = 0.214 * km

{-|
  Estimated km distance for two points in lat-lon format.
  Adopted from
    <https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula>
    by Salvador Dali
-}
latlonDist_km (lat1,lon1) (lat2,lon2) = 12742 * asin (sqrt a)  -- 2*R*asin
    where p = 0.017453292519943295                             -- Pi/180
          a = 0.5
              - (cos ((lat2 - lat1) * p) / 2.0)
              + (cos (lat1 * p)) * (cos (lat2 * p))
              * (1.0 - (cos ((lon2 - lon1) * p))) / 2.0

-- | Create a new gene pool of size n.
createGenePool :: (MonadRandom m) => Int -> [NodeLbl] -> m [[NodeLbl]]
createGenePool n = replicateM n . shuffle

-- | Gets the best gene in the pool -- the one whose route has the highest points.
bestInGenePool :: Graph -> [Gene] -> Gene
bestInGenePool cities = maximumBy (comparing $ geneDist cities)

-- | Gets the worst gene in the pool -- the one whose route has the lowest points.
worstInGenePool :: Graph -> [Gene] -> Gene
worstInGenePool cities = minimumBy (comparing $ geneDist cities)

-- | Gets the stats of a given gene pool.
genePoolStats :: Graph -> [Gene] -> (Float,Float,Float)
genePoolStats cities [] = (0, 0, 0)
genePoolStats cities genes = 
    -- sorted from lowest to highest: that is, worst to best.
    let sorted = sort $ fmap (geneDist cities) genes
        worst  = head sorted
        median = sorted !! (length sorted `div` 2)
        best   = sorted !! (length sorted - 1) in
    (best, median, worst)

-- | Gene selection.
tournamentSelection :: Graph -> Int -> [Gene] -> [Gene]
tournamentSelection _ _ [] = []
tournamentSelection cities n xs = 
    let competitors = take n xs
        xsRemainder = drop n xs
        winner = bestInGenePool cities competitors in 
    winner : (tournamentSelection cities n xsRemainder)

-- | Order 1 crossover 
-- | reference:
-- |   <http://www.rubicite.com/Tutorials/GeneticAlgorithms/CrossoverOperators
-- |     /Order1CrossoverOperator.aspx>
order1Crossover :: (MonadRandom m) => Gene -> Gene -> m Gene
order1Crossover p1 p2 = do
    -- 1. Choose a 'swath' from p1. (random length and starting position)
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
    -- pair up gs and repeat n times, getting the parentPair array.
    let parentPair = foldl (++) [] $ take n $ repeat $ pairup gs
    forM parentPair $ uncurry order1Crossover
    where
        pairup :: [a] -> [(a,a)]
        pairup [] = []
        pairup (x:y:zs) = (x,y) : (pairup zs)
        pairup (x:zs) = (x,x) : (pairup zs)

-- | Mutate the given gene with the given probability `p_swap`.
-- | There are 3 possible mutations: Swap, Insert, and Remove. Each operation
-- | has `p_swap` probability of occurring.
mutate :: (MonadRandom m) => Graph -> Float -> Gene -> m Gene
mutate g p_swap gene = do
    gene'   <- probabilitySwap   g gene
    gene''  <- probabilityInsert g gene'
    gene''' <- probabilityRemove  g gene''
    return $ gene'''

    where
        probabilitySwap :: (MonadRandom m) => Graph -> Gene -> m Gene
        probabilitySwap g gene = do
            p <- getRandom
            if p < p_swap
                then do
                    i1 <- getRandomR (0, length gene - 1)
                    i2 <- getRandomR (0, length gene - 1)
                    return $ swapElementsAt i1 i2 gene
                else return $ gene

        probabilityRemove :: (MonadRandom m) => Graph -> Gene -> m Gene
        probabilityRemove g gene = do
            p <- getRandom
            if p < p_swap
                then do
                    i <- getRandomR (0, length gene - 1)
                    if gene !! i == 1
                        then return $ gene
                        else return $ removeElementAt i gene
                else return $ gene
                 
        probabilityInsert :: (MonadRandom m) => Graph -> Gene -> m Gene
        probabilityInsert g gene = do
            p <- getRandom
            if p < p_swap
                then do
                    let geneSize = Map.size g
                    i <- getRandomR (0, length gene - 1)
                    addUniqueElementAt i geneSize gene
                else return $ gene

        swapElementsAt :: Int -> Int -> [a] -> [a]
        swapElementsAt a b list 
            | a == b = list
            | a > b = swapElementsAt b a list
            | otherwise = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
            where   list1 = take a list;
                    list2 = drop (succ a) (take b list);
                    list3 = drop (succ b) list

        removeElementAt :: Int -> [a] -> [a]
        removeElementAt i list = (take i list) ++ (drop (succ i) list)

        -- | We must take care not to insert an index that already exists in
        -- | the gene.
        addUniqueElementAt :: MonadRandom m => Int -> Int -> Gene -> m Gene
        addUniqueElementAt i geneSize ns = do
            -- The baseSet is a Set that contains the list of all possible
            -- indices for this gene, which given.
            let baseSet = Set.fromList [1 .. geneSize] 

            -- The geneSet is a Set that contains the indices that actually
            -- occurr in the given gene.
            let geneSet = Set.fromList ns

            -- We difference them, hence getting the list of indices that we
            -- can choose from.
            let base = Set.toList $ Set.difference baseSet geneSet
            --traceM $ show base
            case length base of
                0 -> return ns
                _ -> do
                    -- select which element to add. 'base !! n' is the actual element.
                    n <- getRandomR (0, length base - 1)
                    --traceM $ " --> n=" ++ show n
                    --traceM $ " --> i=" ++ show i
                    return $ (take i ns) ++ [base !! n] ++ (drop i ns)
            
mutateN :: (MonadRandom m) => Graph -> Float -> [Gene] -> m [Gene]
mutateN g p genes = forM genes (mutate g p)

generation :: (MonadRandom m) => Graph -> [Gene] -> Float -> m [Gene]
generation graph g0 r = do
    let best_g0 = bestInGenePool graph g0
    let g1 = tournamentSelection graph 3 g0

    offsprings    <- crossover 5 g1
    offsprings_mu <- mutateN graph r offsprings
    best_mu       <- mutateN graph r $ replicate 5 best_g0
    let offsprings_mu' = offsprings_mu ++ [best_g0] ++ best_mu

    let d = (length g0) - (length offsprings_mu')
    shuffle $ offsprings_mu' ++ (replicate d best_g0)

generationN :: (RandomGen g) =>
    Graph
    -> (String -> IO ())
    -> (IORef Bool)
    -> [Gene]
    -> Int
    -> RandT g IO ()

generationN cities printer stopflag g0 ct = do
    stop <- liftIO $ readIORef stopflag
    if stop then return () else do
        let (best, median, worst) = genePoolStats cities g0
        liftIO $ printer $
            (show $ geneDist cities $ bestInGenePool cities g0) ++ ";" ++
            (show $ median) ++ ";" ++
            (show $ worst) ++ ";" ++
            (show $ bestInGenePool cities g0) ++ ";" ++
            (show ct)

        --traceM $ "best-median=" ++ (show $ best - median)
        if best - median < 1.0
            then do
                g1 <- generation cities g0 0.9
                generationN cities printer stopflag g1 (ct+1)
            else do
                g1 <- generation cities g0 0.3333
                generationN cities printer stopflag g1 (ct+1)

start :: (RandomGen g) =>
    Graph
    -> (String -> IO ())
    -> (IORef Bool)
    -> RandT g IO ()
start cities printer stopflag = do
    genes <- createGenePool 1000 (listCities cities)
    generationN cities printer stopflag genes 0

run :: Graph -> (String -> IO ()) -> (IORef Bool) -> IO ()
run g printer stopflag = do
    seed <- (round . (* 1000)) <$> getPOSIXTime 
    genePool <- evalRandT (start g printer stopflag) (mkStdGen seed)
    return ()

