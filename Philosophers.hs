module Philosophers (
    Philosopher,
    philosophersDinner
) where


import System.IO (hFlush, stdout)
import System.Random
import Control.Monad (forever, forM, replicateM)
import Control.Concurrent (threadDelay, forkIO, yield)
import Control.Concurrent.STM


type OutputLog = TChan String

type Spoon = TMVar ()
type Philosopher = String


eat :: Philosopher -> Spoon -> Spoon -> STM ()
eat _ ls rs = takeTMVar ls >> takeTMVar rs

think :: Philosopher -> Spoon -> Spoon -> STM ()
think _ ls rs = putTMVar ls () >> putTMVar rs ()

philosopherLoop :: Philosopher -> Spoon -> Spoon -> OutputLog -> IO ()
philosopherLoop phil ls rs olog = forever (doSomething >> randomThreadDelay)
  where doSomething = atomically $ tryEating `orElse` tryThinking
        tryEating   = eat   phil ls rs >> (writeTChan olog $ phil ++ " eats")
        tryThinking = think phil ls rs >> (writeTChan olog $ phil ++ " thinks")


philosophersDinner :: [Philosopher] -> IO ()
philosophersDinner phils = do
    olog <- newTChanIO

    spoons <- replicateM (length phils) newEmptyTMVarIO
    let spoonPairs = zip (spoons) (tail $ cycle spoons)
    forM (zip phils spoonPairs) $ \(phil, (ls, rs)) -> do
        putStrLn $ phil ++ " joins the eternal dinner"
        forkIO $ philosopherLoop phil ls rs olog

    forever $ printOutputLog olog

printOutputLog olog = atomically (readTChan olog) >>= putStrLn >> hFlush stdout

randomThreadDelay = do
    seconds <- getStdRandom $ randomR (1,9)
    threadDelay $ seconds * 100000

