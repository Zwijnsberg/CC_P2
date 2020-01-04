module RoutingTable where

import Control.Concurrent.STM
import Control.Concurrent
import System.IO
import Data.IORef
import Network.Socket


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

initTable :: Int -> MVar [Node] -> IO RoutingTable
initTable n var = 
  do nodes <- takeMVar var
     local <- newEntry 0 (LocalNode n) (LocalNode n)
     edge  <- mapM (newEntry_ 1) nodes
     putMVar var nodes
     let total =  local : edge
     return $ Table total



sendInitTable :: Int -> [Node] -> IO ()
sendInitTable _ []      = return ()
sendInitTable me (x:xs) =  
    case x of
        (Node y handle _) -> 
            do hPutStrLn handle ("U " ++ (show me) ++ " 0 "++ (show y)) -- Update me cost neighbour
               sendInitTable me xs
        _ -> sendInitTable me xs


                                      

                        
check4Connection :: Node -> RoutingTable -> IO (Bool, Maybe Entry)
check4Connection v (Table [])     = return (False, Nothing) 
check4Connection v (Table (e:ex)) = do
                                     entry <- atomically $ takeTMVar e
                                     if  v == (\(Entry n _ _) -> n) entry
                                         then do 
                                             return (True, Just entry) -- true means: the entry's node is an existing connection within the current routing table
                                         else do
                                             res <- check4Connection v (Table ex)
                                             return res


checkLocal :: Node -> RoutingTable -> IO Bool
checkLocal v (Table (e:_)) = do ex <- atomically $ readTMVar e
                                let ee = (\(Entry n _ _) -> n) ex
                                if v == ee
                                    then do
                                        return True
                                    else do
                                        return False


compareDist :: Int -> Entry -> Bool
compareDist d (Entry _ localD _) | (d+1) < localD    = True
                                 | otherwise         = False


{-
updateDist :: TMVar Entry -> Node -> Node -> Int -> RoutingTable -> RoutingTable
updateDist en n1 n2 dist t@(Table (e:ex)) = return t -}


updateDist :: TMVar Entry -> Node -> Node -> Int -> RoutingTable -> IO RoutingTable
updateDist en n1 n2 dist (Table (e:ex)) = do 
                                            q <- atomically $ newTMVar (Entry n1 (dist+1) n2)
                                            edge <- updateEdge ex q en
                                            let total = e : edge --first is not updated
                                            return $ Table total 


updateEdge :: [TMVar Entry] -> TMVar Entry -> TMVar Entry -> IO [TMVar Entry]
updateEdge [] newE en     = return [newE]
updateEdge (e:ex) newE en = do ee <- atomically $ readTMVar e
                               enx <- atomically $ readTMVar en
                               if ee == enx
                                   then do
                                       q <- atomically $ newTMVar (newE:ex)
                                       qq <- atomically $ readTMVar q
                                       return qq
                                   else do
                                       er <- (updateEdge ex newE en)
                                       let erx = e:er
                                       return erx


removeEntry :: IORef RoutingTable -> MVar [Node] -> Int -> IO ()
removeEntry t n port = do 
                        nodes <- takeMVar n
                        let nodes' = filter (\(Node x _ _) -> x /= port) nodes
                        if (length nodes) -1 == length nodes'
                            then do putStrLn $ "Disconnected: " ++ show port
                            else putStrLn $ "Port " ++ (show port) ++ " is not known"


addEntry :: IORef RoutingTable -> MVar [Node] -> Int -> IO ()
addEntry t n port = do
                -- create the node
                (node:_) <- createNodes [port]

                -- Notify user
                putStrLn $ "Connected: " ++ (show port)

                -- create routing table entry
                newEntry <- atomically $ newTMVar (Entry node 1 node)

                -- add node to node list
                nodes <- takeMVar n
                putMVar n (node : nodes)

                -- add entry to the table
                (Table e) <- readIORef t
                writeIORef t $ Table (newEntry : e)


unMaybe :: Maybe a -> a
unMaybe (Just a) = a