
module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket
import Data.List.Split

import Models
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

sendMessage :: Client -> String -> IO ()
sendMessage (Client _ h) s = hPutStrLn h s

createClients :: [Int] -> IO [Client]
createClients [] = return []
createClients (x:xs) = do client <- createClient x
                          rest   <- createClients xs
                          return $ client : rest

createClient :: Int -> IO Client
createClient n = 
  do putStr $ "Connecting to neighbour " ++ show n ++ " ... "
     client <- connectSocket n
     chandle <- socketToHandle client ReadWriteMode
     putStrLn "connected"
     return (Client n chandle)

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

listenForConnections :: (IORef RoutingTable) -> Socket -> IO ()
listenForConnections t serverSocket = do
  (connection, _) <- accept serverSocket
  _               <- forkIO $ handleConnection t connection
  listenForConnections t serverSocket

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

handleConnection :: (IORef RoutingTable) -> Socket -> IO ()
handleConnection t connection =
  do chandle <- socketToHandle connection ReadWriteMode
     handleConnection' chandle
  where handleConnection' handle = 
          do msg <- hGetLine handle
             t'  <- readIORef t
             handleCommand t' $ parseCommand msg
             handleConnection' handle