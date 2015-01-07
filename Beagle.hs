{-# LANGUAGE BangPatterns #-}

module Beagle where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding (flip)
import System.Random

populationSize =    500     -- The number of candidates in each generation.
tournamentSize =    10      -- The sample size to use when picking random
                            -- candidates for a genertic operation. The smaller
                            -- the size, the more tolerance for weaker 
                            -- candidates.
pMutates =          0.0001  -- During a mutation operation, the probability that
                            -- a given gene will be mutated.
pCrossover =        0.1     -- During a crossover operation, the probability
                            -- that a given gene will cause the dominant parent
                            -- to be switched.
pOpMutate =         0.05    -- The probability that the 'mutation' genetic
                            -- operator will be applied during the evolution
                            -- phase.
pOpReplica =        0.05    -- See above.
pOpCrossover =      0.9     -- See above.

type Scenario =     [Int]   -- 9-dimensional vector representing game scenario.
type Alleles =      [Int]   -- List of optimum moves (0-8) in response to each
                            -- unique game scenario.

-- DOMAIN ----------------------------------------------------------------------

emptyScenario = replicate 9 0

-- | Return whether a scenario represents a game that ended in a draw.
isDraw :: Scenario -> Bool
isDraw g = filter (==0) g == []

-- | Return whether a scenario represents a game that ended by player X (1) 
-- winning.
isWinner :: Scenario -> Bool
isWinner [1, 1, 1, _, _, _, _, _, _] = True
isWinner [_, _, _, 1, 1, 1, _, _, _] = True
isWinner [_, _, _, _, _, _, 1, 1, 1] = True
isWinner [1, _, _, 1, _, _, 1, _, _] = True
isWinner [_, 1, _, _, 1, _, _, 1, _] = True
isWinner [_, _, 1, _, _, 1, _, _, 1] = True
isWinner [1, _, _, _, 1, _, _, _, 1] = True
isWinner [_, _, 1, _, 1, _, 1, _, _] = True
isWinner _ = False

-- | Replace 1s with -1s and vice versa. Useful for use with 'isWinner'.
invertPlayers :: Scenario -> Scenario
invertPlayers = map (\x -> case x of 1 -> (-1); (-1) -> 1; x -> x)

-- | Given a scenario return the player who needs to make the next move. 
--
-- Possible values are 1 or -1. This can be determined easily by counting the 
-- number of 1s and -1s present in the scenario and knowing that 1 always goes 
-- first.
detectTurn :: Scenario -> Int
detectTurn s = case (numX == numO) of True -> 1; False -> (-1)
    where numX = count 1
          numO = count (-1)
          count x = length $ filter (==x) s

-- | Given a list of scenarios, transform each scenario to its base case and 
-- return a list of unique base cases.
toBases :: [Scenario] -> [Scenario]
toBases = nub . map (fst . toBase)

-- | Transform a single scenario to its base case.
--
-- A 'restore' function is also returned to transform the base case back to 
-- its original form.
toBase :: Scenario -> (Scenario, Scenario -> Scenario)
toBase = getBase . variants

-- | Given a list of 8 symmetrical scenarios, pick the base case (the one with 
-- the lowest lexicographical ordering).
--
-- Lexicographical order proved more efficient than hashing each scenario and 
-- picking the one with the lowest hash - and is sufficient; we just need an
-- arbitrary but deterministic member of the varitions to act as the base.
getBase :: Ord a => [(a, b)] -> (a, b)
getBase = head . sortBy (\(a, _) (b, _) -> compare a b) 

-- | Return whether a scenario represents a completed game (ie. there has been
-- a win or draw).
hasScenarioEnded :: Scenario -> Bool
hasScenarioEnded s 
    | isWinner s =                  True
    | isWinner $ invertPlayers s =  True
    | isDraw s =                    True
    | otherwise =                   False

-- | Given a scenario return all 8 symmetrical variations along with a function
-- that can be applied to any of the variations to restore the original 
-- scenario.
--
-- This 'restore' function is used to transform a base scenario to its original
-- form after a mark has been placed.
variants :: Scenario -> [(Scenario, Scenario -> Scenario)]
variants s = map (\(f, rev) -> (f s, rev)) fs
    where fs = [
            (id,                                id),
            (rotate,                            rotate. rotate. rotate),
            (rotate. rotate,                    rotate. rotate),
            (rotate. rotate. rotate,            rotate),
            (flip,                              flip),
            (rotate. flip,                      flip. rotate. rotate. rotate),
            (rotate. rotate. flip,              flip. rotate. rotate),
            (rotate. rotate. rotate. flip,      flip. rotate)
            ]

-- | Rotate a scenario 90 degrees clockwise.
-- 
-- a b c      g d a
-- d e f  ->  h e b
-- g h i      i f c
rotate :: Scenario -> Scenario
rotate (a:b:c:d:e:f:g:h:i:[]) = [g, d, a, h, e, b, i, f, c]

-- | Flip a scenario over its horizontal axis.
--
-- a b c      g h i
-- d e f  ->  d e f
-- g h i      a b c
flip :: Scenario -> Scenario
flip (a:b:c:d:e:f:g:h:i:[]) = [g, h, i, d, e, f, a, b, c]

-- | Given a scenario return all the subsequent scenarios that can be generated
-- by placing a single player mark.
immScenarios :: Scenario -> [Scenario]
immScenarios s
    | hasScenarioEnded s = []
    | otherwise = filter (not . hasScenarioEnded) $ allMoves s 

-- | Given a scenario return all possible subsequent scenarios that can be 
-- generated by playing the scenario through to the end of a game, for every
-- possible sequence of moves.
subScenarios :: Scenario -> [Scenario]
subScenarios s = s : (nub . concat . map subScenarios $ immScenarios s)

-- HASH TABLE ------------------------------------------------------------------

-- | Express a scenario as an int, for use as a hash table key.
toKey :: Scenario -> Int
toKey s = foldr toKey' 0 (zip [0..] (positive s))
    where toKey' (i, x) b = b + (x * (10 ^ i))
          positive = map (\x -> case x of (-1) -> 1; 0 -> 2; 1 -> 3)

-- | Build a table of gene index positions, keyed by base scenarios.
buildTable :: Map.Map Int Int
buildTable = Map.fromList $ zip scenarioKeys [0..] 
    where scenarioKeys = map toKey $ subScenarios emptyScenario

-- | Return the gene index associated with a base scenario.
scenarioIndex :: Scenario -> Int
scenarioIndex s = case Map.lookup (toKey s) buildTable of
                      (Just i) -> i
                      Nothing ->  error "Illegal scenario"

-- | Using a list of alleles (genetically evolved) solve a scenario by making
-- the optimum next move.
--
-- First convert the scenario to its base case (and restore function), then
-- lookup the optimum move for the base case in the alleles list, apply the
-- optimum move then transform the modified base case to its original state
-- (using the restore function).
solve :: Alleles -> Scenario -> Scenario
solve a s = let (base, restore) = toBase s
                i = scenarioIndex base
                a' = playMove base (a !! i)
            in restore a'

-- FITNESS EVALUATION ----------------------------------------------------------

-- | Given a list of game outcomes (success/failure) return a float score.
score :: [Bool] -> Float
score xs = (realToFrac total - realToFrac didntLose) / realToFrac total
    where total = length xs
          didntLose = length $ filter (==True) xs

-- Play all possible games against the genome (with the genome playing first as
-- 'X' then second as 'O') and return a score reflecting all outcomes.
evalFitness :: (Scenario -> Scenario) -> IO Float
evalFitness gnm = fmap score $ evalFitness' gnm 
    where evalFitness' gnm = do
              xFst <- playGame gnm 1 emptyScenario
              xSnd <- playGame gnm (-1) emptyScenario
              return (xFst++xSnd)

-- | Given a game scenario and genome play all possible variations of the game
-- return a list of outcomes.
--
-- When it's the genome's turn, the genome will play what it believes is the
-- optimum move (given the scenario). When it's the hosts's turn, the host will
-- play every possible move.
playGame :: (Scenario -> Scenario)  -- genome function
         -> Int                     -- mark being used by genome: 1 or (-1)
         -> Scenario                -- current game scenario
         -> IO [Bool]               -- list of outcomes of all possible games
playGame gnm m s 
    | isWinner s =                      return [m == 1]
    | isWinner $ invertPlayers s =      return [m == (-1)]
    | isDraw s =                        return [True]
    | otherwise =
        case (detectTurn s == m) of
            True ->  playGame gnm m (gnm s) -- genome play
            False -> fmap concat . sequence $ 
                     map (playGame gnm m) (allMoves s) -- host play

-- | Place a mark in a scenario at a given (modulated) position.
playMove :: Scenario -> Int -> Scenario
playMove s i = let (x, _:xs) = splitAt (free !! i') s in x ++ (detectTurn s:xs)
    where free = map snd $ filter (\x -> fst x == 0) (zip s [0..])
          i' = i `mod` (length free)

-- | Return all (base) legal moves that can be made from a scenario.
allMoves :: Scenario -> [Scenario]
allMoves s = toBases $ map (playMove s) [0..8]

-- EVOLUTION -------------------------------------------------------------------

-- | Evolve a population into a new generation of equal size.
--
-- A population is evolved by applying the genetic operators 'replica', 'mutate' 
-- and 'crossover' according to probability distribution 'probDist'.
--
-- Each operator will employ 'tournament selection' to pick its operands.
-- (see 'tselect')

evolve :: [(Alleles, Float)] -> IO [Alleles]
evolve as = evolve' as populationSize

evolve' _ 0 = return []
evolve' as n = do
    r   <- randomRIO (0.0,1.0)
    a   <- (operator r) as
    as' <- evolve' as (n-1)
    return (a:as')
    where operator = pick (toCumProb probDist)
          pick ((f, p):xs) r = if r <= p then f else pick xs r
          probDist = [
              (mutate,      pOpMutate),
              (replica,     pOpReplica),
              (crossover,   pOpCrossover)
              ]

toCumProb :: [(f, Float)] -> [(f, Float)]
toCumProb dist = let cum = tail $ scanl (+) 0 (map snd dist)
                 in zip (map fst dist) cum

-- | Genetic Operator: Replicate an individual (unchanged).
replica = tselect

-- | Genetic Operator:  Mutate a random number of genes in an individual.
mutate :: [(Alleles, Float)] -> IO Alleles
mutate as = do
    a <- tselect as
    mutate' a

mutate' :: Alleles -> IO Alleles
mutate' [] = return []
mutate' (x:xs) = do
    outcome <- happen pMutates
    x' <- case outcome of
              True ->  randomRIO (0,8)
              False -> return x
    xs' <- mutate' xs
    return (x':xs')

-- | Genetic Operator: Breed two parents (using multi-point crossover) to 
-- produce a single offspring. 
crossover :: [(Alleles, Float)] -> IO Alleles
crossover as = do
    parentA  <- tselect as
    parentB  <- tselect as
    dominant <- randomRIO (False,True)
    crossover' (zip parentA parentB) dominant

crossover' :: [(a, a)] -> Bool -> IO [a]
crossover' [] _ = return []
crossover' ((a, b):xs) dominant = do
    switchDominant <- happen pCrossover
    xs' <- crossover' xs (if switchDominant then not dominant else dominant)
    let !x = get a b dominant
    return (x:xs')
    where get a b True =  a
          get a b False = b

-- | Pick an individual from a population by first picking a sample, then 
-- selecting the individual with the best fitness.
tselect :: [([a], Float)] -> IO [a]
tselect as = do
    s <- sample as tournamentSize
    return (fst . head $ sort' s)
    where sort' = sortBy (\a b -> compare (snd a) (snd b))

-- | Given an event probability and pseudo random number generator return 
-- whether the event "happened".
happen :: Float -> IO Bool
happen x = fmap (<=x) $ randomRIO (0.0,1.0)

-- | Return n random elements from a list.
sample :: [a] -> Int -> IO [a]
sample _ 0 = return []
sample xs n = do
    i   <- randomRIO (0, (length xs)-1)
    xs' <- sample (remove xs i) (n-1)
    return ((xs !! i):xs')
    where remove xs i = let (a, b) = splitAt i xs in a ++ (drop 1 b)

-- RUNNER ----------------------------------------------------------------------

-- | Create a random candidate (list of alleles).
seedAlleles :: IO Alleles
seedAlleles = seedAlleles' (Map.size buildTable)
    where seedAlleles' 0 = return []
          seedAlleles' n = do
              x  <- randomRIO (0,8)
              xs <- seedAlleles' (n-1)
              return (x:xs)

-- | Create a random population (list of candidates).
seedPopulation :: IO [Alleles]
seedPopulation = seedPopulation' populationSize
    where seedPopulation' 0 = return []
          seedPopulation' n = do
              x  <- seedAlleles
              xs <- seedPopulation' (n-1)
              return (x:xs) 

-- | Assign a fitness score (0-1) to each candidate in the population, where
-- a score of 0 indicates target behaviour.
scorePopulation :: [Alleles] -> IO [(Alleles, Float)]
scorePopulation = fmap sort . sequence . map score
    where sort = sortBy (\a b -> compare (snd a) (snd b))
          score x = do
              s <- evalFitness (solve x)
              return (x, s)

-- Given a list of scored alleles, return the first that exhibits target
-- behaviour or Nothing.
solutions :: [(Alleles, Float)] -> Maybe Alleles
solutions as = case (map fst . filter (\x -> snd x == 0) $ as) of
                   [] ->     Nothing
                   (a:as) -> Just a

-- Successively evolve a population of candidates until a candidate exhibiting
-- target behaviour emerges.
search :: [Alleles] -> IO Alleles
search !as = do
    sas <- scorePopulation as
    case (solutions sas) of
        (Just a) -> return a
        Nothing -> do
            as' <- evolve sas
            search as'
