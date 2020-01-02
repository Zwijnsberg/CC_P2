module RoutingTable where

import Control.Concurrent.STM
import System.IO

import Models

printEntry :: (TMVar Entry) -> IO ()
printEntry var = do entry <- atomically $ readTMVar var
                    putStrLn $ show entry

printRoutingTable :: RoutingTable -> IO ()
printRoutingTable (Table []) = return ()
printRoutingTable (Table (x:xs)) = do printEntry x
                                      printRoutingTable $ Table xs

newEntry :: Int -> Node -> Node -> IO (TMVar Entry)
newEntry value c2 c1 = atomically $ newTMVar $ Entry c1 value c2

newEntry_ :: Int -> Node -> IO (TMVar Entry)
newEntry_ value c1 = atomically $ newTMVar $ Entry c1 value c1

initTable :: Int -> [Node] -> IO RoutingTable
initTable n xs = 
  do local <- newEntry 0 (LocalNode n) (LocalNode n)
     edge  <- mapM (newEntry_ 1) xs
     let total =  local : edge
     return $ Table total

sendInitTable :: Int -> [Node] -> IO ()
sendInitTable _ []      = return ()
sendInitTable me (x:xs) =  
    case x of
        (Node y handle) -> 
            do hPutStrLn handle ("U " ++ (show me) ++ " 0 "++ (show y)) -- Update me cost neighbour
               sendInitTable me xs
        _ -> sendInitTable me xs