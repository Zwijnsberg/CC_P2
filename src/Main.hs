
module Main where

import Control.Concurrent
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket

import Models
import CommandHandler
import ConnectionHandler
import RoutingTable


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- me :: Int is the port number of this process
  -- neighbours :: [Int] is a list of the port numbers of the initial neighbours
  -- During the execution, connections may be broken or constructed
  (me, neighbours) <- readCommandLineArguments

  putStrLn $ "I should be listening on port " ++ show me
  putStrLn $ "My initial neighbours are " ++ show neighbours

  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ portToAddress me
  listen serverSocket 1024
  -- Let a seperate thread listen for incomming connections

  -- create empty table
  table <- newIORef $ Table []

  -- create thread to listen for connections
  _ <- forkIO $ listenForConnections table serverSocket

  -- create nodes
  clients <- createClients neighbours

  -- create table and update 
  table'   <- initTable me clients
  writeIORef table table'

  --msgs <- getTheMessages table'
  sendInitTable me clients  --msgs

  -- start thread to listen for user input
  _ <- forkIO $ listenForCommandLine table

  
  threadDelay 1000000000






