{- Process that reads Tic-Tac-Toe game scenarios from stdin and responds to 
them using stdout.
  
Game scenarios are encoded as integers (see Beagle.hs). The response (on 
stdout) is an encoded game scenario prefixed by an integer representing the 
current state of the game (see State).

Eg:
222222222 => 3222232222
-}

import Beagle
import Control.Monad
import qualified Data.Map as Map
import System.IO

-- | Computer player function that responds to game scenarions.
--
-- The function pre-applies the "perfect" evolved DNA to the 'solve' function. 
genome :: Scenario -> Scenario
genome = solve [4,4,0,2,5,5,5,7,5,7,3,3,4,1,0,1,5,4,5,5,0,6,0,4,1,3,8,5,8,1,4,3,0,1,3,6,3,6,6,3,4,2,3,6,1,7,5,6,4,2,3,5,6,0,0,1,5,2,3,5,8,7,8,4,7,4,7,3,4,8,5,2,6,2,7,3,5,2,7,2,4,1,1,1,2,0,3,0,7,4,1,7,0,1,0,3,6,5,1,6,3,0,3,7,6,8,5,2,5,5,5,8,7,1,7,1,4,7,5,8,5,4,3,3,7,6,8,5,0,7,5,8,2,5,5,0,6,1,1,5,7,5,8,1,1,8,6,4,8,0,0,6,4,3,3,6,2,1,7,5,2,1,7,7,3,1,6,0,7,0,3,3,4,7,1,2,6,8,7,0,3,4,7,5,2,4,2,6,8,7,0,6,6,2,3,7,3,3,7,5,0,5,7,3,7,0,8,7,2,7,6,6,7,3,4,8,5,6,1,6,8,0,7,5,3,2,5,4,7,4,8,3,2,1,5,3,0,7,4,8,1,7,4,2,8,3,5,6,6,7,3,7,0,2,6,1,7,3,6,0,6,5,3,0,0,7,5,2,8,4,6,2,7,4,0,1,5,3,8,4,2,3,7,0,6,5,1,0,7,5,6,8,7,2,6,8,6,4,3,7,4,5,3,3,7,7,5,6,2,8,1,7,4,0,2,6,0,2,0,3,4,7,7,6,6,4,5,3,6,8,4,6,4,0,1,2,4,4,8,1,4,8,7,4,0,3,1,5,5,8,2,8,7,1,2,1,7,8,5,3,0,4,8,4,8,7,2,0,0,5,6,8,1,7,2,5,1,5,5,7,0,4,8,1,6,1,1,2,8,3,4,6,4,4,3,0,0,7,3,3,6,2,3,4,5,7,6,8,1,4,0,8,5,0,5,5,2,8,5,8,7,4,0,6,4,7,4,8,0,5,6,2,3,0,1,5,6,4,8,5,7,4,5,0,5,4,4,5,8,6,3,0,2,0,1,3,8,6,8,8,0,1,7,1,3,8,0,2,1,1,8,1,1,0,7,4,4,0,6,3,7,7,6,5,0,0,1,7,1,3,7,6,0,5,2,8,2,7,8,6,0,1,7,3,7,6,6,2,5,7,6,8,4,6,8,1,8,5,7,5,5,4,2,1,6,8,6,8,2,4,6,4,7,2,1,0,6,2,8,0,4,6,4,8,6,2,1,4,7,2,6,8,1,0,6,5,1,0,5,3,3,4,6,1,5,6,7,6,8,7,5,6,0,0,0,8,7,8,7,4,2,0,4,1,1,0,2,0,8,5,1,3,4,5,7,6,3,8,0,3,2,4,5,4,2,1,0,5,3,6,4,7,4,5,5,7,7,0,5,1,2,5,2,1,8,2,3] 

data State = X_WINS | O_WINS | DRAW | ACTIVE | ILLEGAL
instance Show State where
    show X_WINS =   "0"
    show O_WINS =   "1"
    show DRAW =     "2"
    show ACTIVE =   "3"
    show ILLEGAL =  "4"

-- | Return the current game play state represented by a scenario.
state :: Scenario -> State
state scn
    | isWinner scn =                    X_WINS
    | isWinner $ invertPlayers scn =    O_WINS
    | isDraw scn =                      DRAW
    | isIllegal scn =                   ILLEGAL
    | otherwise =                       ACTIVE

-- | Return whether a scenario is legal (according to the rules of Tic-Tac-Toe).
--
-- It is assumed that the scenario is syntactically correct.
isIllegal :: Scenario -> Bool
isIllegal scn = let base = fst $ toBase scn
                in case Map.lookup (toKey base) buildTable of
                    Just _ ->   False
                    Nothing ->  True

-- | Convert a key to a scenario.
fromKey :: Int -> Scenario
fromKey = reverse . map (\x -> case x of '1' -> (-1); '2' -> 0; '3' -> 1) . 
          show

-- | The "computer" will play a response to a scenario, provided that the 
-- scenario represents an active game.
maybeComputerPlay :: Scenario -> State -> (Scenario, State)
maybeComputerPlay scn ACTIVE = let scn' = genome scn
                               in (scn', state scn')
maybeComputerPlay scn st = (scn, st)

main = forever $ do
    scn <- fmap (fromKey . read) getLine
    let st = state scn
        (scn', st') = maybeComputerPlay scn st
    putStrLn (show st' ++ show (toKey scn'))
    hFlush stdout
