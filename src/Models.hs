module Models where

import Control.Concurrent
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket

import Control.Concurrent.STM
import System.IO

data Entry = Entry Node Int Node -- dest, cost, closest neigh
data RoutingTable = Table [TMVar Entry]
data Node = Node Int Handle | LocalNode Int
data Message = MyDist Node Node Int -- sender dest, cost
data ChangeTable = TMVar Bool

data Command = Show 
             | Send Int String 
             | Make Int
             | Disconnect Int
             | Distance Int Int Int -- distance from neightbour, value, to me
             | Unknown 
          deriving Show

instance Show Node where
  show (LocalNode x) = show x
  show (Node x _)    = show x

instance Show Entry where
  show (Entry (LocalNode x) v (LocalNode _))
    = (show x) ++ " " ++ (show v) ++ " local"
  show (Entry node1 v node2) 
    = (show node1) ++ " " ++ (show v) ++ " " ++ (show node2)



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