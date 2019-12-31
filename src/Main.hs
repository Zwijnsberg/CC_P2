
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

  _ <- forkIO $ listenForConnections table serverSocket

  -- As an example, connect to the first neighbour. This just
  -- serves as an example on using the network functions in Haskell
  case neighbours of
    [] -> putStrLn "I have no neighbours :("
    neighbours -> do
      clients <- createClients neighbours

      -- create table and update 
      table'   <- initTable me clients
      writeIORef table table'
      sendInitTable me clients

      _       <- forkIO $ listenForCommandLine table

      return ()

  threadDelay 1000000000

createClients :: [Int] -> IO [Node]
createClients [] = return []
createClients (x:xs) = do client <- createClient x
                          rest   <- createClients xs
                          return $ client : rest

createClient :: Int -> IO Node
createClient n = 
  do putStr $ "Connecting to neighbour " ++ show n ++ " ... "
     client <- connectSocket n
     chandle <- socketToHandle client ReadWriteMode
     putStrLn "connected"
     return (Node n chandle)

readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, map read neighbours)

portToAddress :: Int -> SockAddr
portToAddress portNumber = SockAddrInet (fromIntegral portNumber) (tupleToHostAddress (127, 0, 0, 1)) -- localhost

connectSocket :: Int -> IO Socket
connectSocket portNumber = connect'
  where
    connect' = do
      client <- socket AF_INET Stream 0
      result <- try $ connect client $ portToAddress portNumber
      case result :: Either IOException () of
        Left _ -> do
          threadDelay 1000000
          connect'
        Right _ -> return client