
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
  _ <- forkIO $ listenForConnections serverSocket

  -- As an example, connect to the first neighbour. This just
  -- serves as an example on using the network functions in Haskell
  case neighbours of
    [] -> putStrLn "I have no neighbours :("
    neighbours -> do
      clients <- createClients neighbours
      _       <- forkIO listenForCommandLine
      return ()
      -- closeCLients clients
      -- client <- connectSocket neighbour
      -- chandle <- socketToHandle client ReadWriteMode
      -- Send a message over the socket
      -- You can send and receive messages with a similar API as reading and writing to the console.
      -- Use `hPutStrLn chandle` instead of `putStrLn`,
      -- and `hGetLine  chandle` instead of `getLine`.
      -- You can close a connection with `hClose chandle`.
      -- hPutStrLn chandle $ "Hi process " ++ show neighbour ++ "! I'm process " ++ show me ++ " and you are my first neighbour."
      -- putStrLn "I sent a message to the neighbour"
      -- message <- hGetLine chandle
      -- putStrLn $ "Neighbour send a message back: " ++ show message
      -- hClose chandle

  threadDelay 1000000000

closeClients :: [Handle] -> IO ()
closeCLients [] = return ()
closeClients (x:xs) = do hClose x
                         closeCLients xs

sendMessage :: Handle -> String -> IO ()
sendMessage h s = hPutStrLn h s

createClients :: [Int] -> IO [Handle]
createClients [] = return []
createClients (x:xs) = do handle <- createClient x
                          rest <- createClients xs
                          let result = handle : rest
                          return result
  -- return [createClient x | x <- xs]

createClient :: Int -> IO Handle
createClient n = 
  do putStrLn $ "Connecting to neighbour " ++ show n ++ "..."
     client <- connectSocket n
     chandle <- socketToHandle client ReadWriteMode
     return chandle

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

listenForConnections :: Socket -> IO ()
listenForConnections serverSocket = do
  (connection, _) <- accept serverSocket
  _               <- forkIO $ handleConnection connection
  listenForConnections serverSocket

parseCommand :: String -> (Command, String)
parseCommand s = case head of 
  "show" -> (Show, concat rest)
  "send" -> (Send, concat rest)
  "make" -> (Make, concat rest)
  "disc" -> (Disconnect, concat rest)
  _      -> (Unknown, concat rest)
  where (head:rest) = splitOn ";" s

listenForCommandLine :: IO ()
listenForCommandLine = do line <- getLine
                          handleCommandLine $ parseCommand line
                          listenForCommandLine

handleCommandLine :: (Command,String) -> IO ()
handleCommandLine (c,s) = putStrLn $ (show c) ++ " " ++ s
 
handleConnection :: Socket -> IO ()
handleConnection connection = do
  putStrLn "Got new incomming connection"
  chandle <- socketToHandle connection ReadWriteMode
  hPutStrLn chandle "Welcome"
  message <- hGetLine chandle
  putStrLn $ "Incomming connection send a message: " ++ message
  hClose chandle

-- Routing table data types
data Command = Show | Send | Make | Disconnect | Unknown deriving Show
data Port = Port Int | Local
data Entry = Entry Port Int Port
data RoutingTable = Table [TMVar Entry]