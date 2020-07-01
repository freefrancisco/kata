{- 
A solution to the Dining Philosophers Problem https://en.wikipedia.org/wiki/Dining_philosophers_problem
using Haskell's Software Transactional Memory https://en.wikipedia.org/wiki/Software_transactional_memory
-}

{-# LANGUAGE NumericUnderscores #-}

module Philosophers where
 
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent as C
import qualified Control.Monad as M
import qualified System.Random as R
 
type Philosopher = String
type SpoonID = Int 
 
-- A Spoon is  TMVar with a SpoonID
type Spoon = STM.TMVar SpoonID

makeNewSpoon :: SpoonID -> IO Spoon
makeNewSpoon = STM.newTMVarIO 

-- | Taking a TMVar makes it empty, blocks others who try to take it until something is put in it
takeSpoon :: Spoon -> STM SpoonID
takeSpoon  = STM.takeTMVar
 
--  | Putting the spoon in a TMVar unblocks others who tried to take it and are waiting for it
releaseSpoon :: Spoon -> SpoonID -> STM ()
releaseSpoon  = STM.putTMVar

-- | Read if a particular spoon is taken or not
isSpoonTaken :: Spoon -> STM Bool
isSpoonTaken = STM.isEmptyTMVar 

-- | wait a random amount between 0 and secs seconds
randomWaitSeconds :: Int -> IO ()
randomWaitSeconds secs = do
    let granularity = 1000 -- more granularity less chance of collision when printing out
    let oneSecond =  round $ 1000_000 / fromIntegral granularity -- 1, 10 seconds. threadDelay uses nanoseconds.
    delay <- R.randomRIO (1, secs * granularity)
    C.threadDelay (delay * oneSecond) 

-- | grab spoons to the left and the right
grabSpoons :: Spoon -> Spoon  -> IO (SpoonID, SpoonID)
grabSpoons leftSpoon rightSpoon = STM.atomically $ do
        leftSpoonID <- takeSpoon leftSpoon
        rightSpoonID <- takeSpoon rightSpoon
        return (leftSpoonID, rightSpoonID)

-- | Print what is going on with the spoons, taken = *, not taken = . 
spoonReport :: [Spoon] -> IO ()
spoonReport spoons = do 
    statuses <- STM.atomically $ mapM isSpoonTaken spoons -- check on all spoons at this point if they are taken or not
    let out = map display statuses
        display True = '*'
        display False = '.'
    putStrLn out

--  | run a philosopher process forever with a philosopher and the spoons to his left and right
runPhilosopher :: Philosopher -> (Spoon, Spoon) -> [Spoon] -> IO ()
runPhilosopher philosopher (leftSpoon, rightSpoon)  spoons = M.forever $ do
    randomWaitSeconds 10
    putStrLn $ philosopher ++ " is hungry."
    spoonReport spoons
    (leftSpoonID, rightSpoonID) <- grabSpoons leftSpoon rightSpoon
    putStrLn $ philosopher ++ " got spoons " ++ show leftSpoonID ++ " and " ++ show rightSpoonID ++ " and is now eating."
    spoonReport spoons
    randomWaitSeconds 10
    putStrLn $ philosopher ++ " is done eating. Going back to thinking."
    STM.atomically $ do
        releaseSpoon leftSpoon leftSpoonID 
        releaseSpoon rightSpoon rightSpoonID 
    spoonReport spoons
 
philosophers :: [Philosopher]
philosophers = ["Aristotle", "Plato", "Kant", "Socrates", "Nietzche", "Descartes", "Bacon", "Russell"]

makeSpoons :: IO [Spoon]
makeSpoons = mapM makeNewSpoon [1..(length philosophers)]
 
main :: IO String
main = do
    spoons <- makeSpoons
    let spoonPairs = zip spoons $ tail $ cycle spoons 
        philosophersWithNames  = map runPhilosopher philosophers
        philosophersWithNamesAndSpoons = zipWith ($) philosophersWithNames spoonPairs
        philosophersWithNamesAndSpoonsAndList = map ($ spoons) philosophersWithNamesAndSpoons
    putStrLn "Running the philosophers. Press enter to exit the program."
    mapM_ C.forkIO philosophersWithNamesAndSpoonsAndList -- run each philosopher process in its own separate thread
    getLine -- This ends the main thread, all forked threads exit when the main thread exits
 