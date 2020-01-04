
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
  
  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ portToAddress me
  listen serverSocket 1024
  -- Let a seperate thread listen for incomming connections

  -- create empty table
  table <- newIORef $ Table []
  
  -- create nodes
  nodes'  <- createNodes neighbours
  mapM (\x -> putStrLn ("Connected: " ++ (show x))) neighbours
  clients <- newMVar $ nodes'

  -- create thread to listen for connections
  _ <- forkIO $ listenForConnections table clients serverSocket

  -- listens to messages                                         
  (handle, host, port) <- accept sock
  forkFinally (talk handle) (\_ -> hClose handle)  

  -- create table and update 
  table'   <- initTable me clients
  writeIORef table table'
  updateNeighbors me table' (takeMVar clients)
      -- Here FUNCTIE THAT DIVERTS RECEIVED MESSAGES (that will provide changes) TO RECOMPUTE FUNCTION, WHICH WILL IN TURN RESEND OUR CHANGES BACK TO OUR NEIGHBROS

  --msgs <- getTheMessages table'
  clients' <- takeMVar clients
  sendInitTable me clients'
  putMVar clients clients'
  
  -- start thread to listen for user input
  _ <- forkIO $ listenForCommandLine table clients
  
  threadDelay 1000000000




updateNeighbors :: Int -> IORef RoutingTable -> [Node] -> IO ()  -- doel: stuur alle neighbors DistMessages in de form van -senderNode node dist--
updateNeighbors me t []     = return ()
updateNeighbors me t (x:xs) =  do 
                                  msgs <- getTheMessages t
                                  case x of
                                      (Node y handle _) -> 
                                          do mapM (hPutStrLn handle) msgs -- Update me cost neighbour
                                             updateNeighbors me t xs
                                      _ -> sendInitTable me xs



listenForMessages :: IO ()
listenForMessages = withSocketsDo $ do
                                      sock <- listenOn (PortNumber (fromIntegral port))              
                                      forever $ do                                                   
                                        (handle, host, port) <- accept sock                         
                                        printf "Accepted connection from %s: %s\n" host (show port)
                                        forkFinally (talk handle) (\_ -> hClose handle)   


talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering                                
  loop                                                         
 where
  loop = do
    line <- hGetLine h                                         
    msg <- toMessage line
    recompute me msg table 



getTheMessages :: RoutingTable -> IO [String]
getTheMessages (Table (e:ex)) = do me <- (\Entry x _ _ -> x) e
                                   localPort <- getPort me
                                   messages <- map (\Entry n d _ -> (show localPort ++ "," ++ show d ++ "," ++ show (getPort n))) ex
                                   return messages

getPort :: Node -> Int
getPort n = (\Node x _ _ -> x) n



toMessage :: String -> Message
toMessage s = [(a,b,c)|[a,b,c] <- lst]
            where lst = splitOn "," s



recompute :: Int -> Message -> IORef RoutingTable -> IO ()
recompute me (v,w,d) t =  do
                                changeTable <- newIORef False
                                n <- getNode w clients
                                nv <- getNode v clients
                                if check4Connection v t
                                    then do
                                        (b,e) <- checkLocal w t
                                        if b
                                            then do
                                                return ()
                                            else do
                                                 if compareDist d (unMaybe e) 
                                                    then do
                                                            newTable <- updateDist e n nv d t
                                                            writeIORef changeTable True
                                                    else do
                                                        return ()
                                else do 
                                        addEntry t n
                                        recompute me (v,w,d) t
                                c <- readIORef changeTable
                                if c == True
                                  then do
                                      writeIORef t newTable
                                      cls <- (takeMVar clients)
                                      updateNeighbors me t cls
                                      return ()
                                  else do
                                      return ()

getNode :: Int -> [Node] -> Maybe Node
getNode i [] = Nothing
getNode i [c:cx] | i == (\(Node x _ _) -> x) = c
                 | otherweise              = getNode i cx