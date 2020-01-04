module CommandHandler where

import System.IO
import Data.IORef
import Control.Concurrent
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

listenForCommandLine :: IORef RoutingTable -> MVar [Node] -> IO ()
listenForCommandLine t n = do line <- getLine
                              handleCommand t n $ parseCommand line
                              listenForCommandLine t n

handleCommand :: IORef RoutingTable -> MVar [Node] -> Command -> IO ()
handleCommand t _ Show             = do t' <- readIORef t
                                        printRoutingTable t'
handleCommand t _ (Send p m)       = undefined
handleCommand t n (Make p)         = addEntry t n p                       
handleCommand t n (Disconnect p)   = removeEntry t n p
handleCommand t _ (Distance n d m) = putStrLn "updating distance"
handleCommand t _ cmd              = putStrLn $ "Undefined or Unspecified command entered ... " ++ (show cmd) 


