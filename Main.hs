import Beagle
import System.Random

main = do 
    setStdGen (mkStdGen 0)
    p <- seedPopulation
    t <- search p
    print t
