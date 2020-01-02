module CommandHandler where

import Data.IORef
import Data.List.Split

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
                            t'   <- readIORef t
                            handleCommand t' $ parseCommand line
                            listenForCommandLine t

handleCommand :: RoutingTable -> Command -> IO ()
handleCommand t Show = printRoutingTable t
handleCommand t (Distance n d m) = putStrLn "updating distance"
handleCommand t cmd = putStrLn $ "Undefined or Unspecified command entered ... " ++ (show cmd) 