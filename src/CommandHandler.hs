module CommandHandler where

import System.IO
import Data.IORef
import Data.List.Split
import Network.Socket


import Models
import RoutingTable

parseCommand :: String -> Command
parseCommand s = case cmd of 
  "R" -> Show
  "B" -> Send (getPort r) (getMsg r)
  "C" -> Make (getPort r)
  "D" -> Disconnect (getPort r)
  "U" -> getDist r
  _   -> Unknown
  where (cmd:r) = splitOn " " s
        getPort (p:_) = read p :: Int
        getMsg  (_:m) = concat m
        getDist (neighbour:dist:me:_) = Distance (read neighbour :: Int) (read dist :: Int) (read me :: Int)

listenForCommandLine :: (IORef RoutingTable) -> IO ()
listenForCommandLine t = do line <- getLine
                            handleCommand t $ parseCommand line
                            listenForCommandLine t

handleCommand :: (IORef RoutingTable) -> Command -> IO ()
handleCommand t Show        = do 
                                t' <- readIORef t
                                printRoutingTable t'
handleCommand t (Send p m)  = undefined
handleCommand t (Make p)    = addEntry t p                       


{-handleCommand t (Disconnect p) = do
                                    shutdown p ShutdownBoth
                                    removeEntry p t -}
                                    
handleCommand t (Distance n d m) = putStrLn "updating distance"
handleCommand t cmd = putStrLn $ "Undefined or Unspecified command entered ... " ++ (show cmd) 


