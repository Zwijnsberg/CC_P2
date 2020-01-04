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

{-


getTheMessages :: RoutingTable -> Messages
getTheMessages (Table (e:ex)) = do local <- (\Entry x _ _ -> x) edge
                                messages <- mapM (\Entry x y z -> MyDist local x y) xs
                                sendInitTable messages


sendInitTable :: [Message] -> [Node] -> IO ()
sendInitTable _ []      = return ()
sendInitTable (x:xs) = do 
                        let nod = (Node x handle)
                        hPutStrLn handle ("U " ++ (show me) ++ " 0 "++ (show y)) -- Update me cost neighbour
                        sendInitTable me xs
                        return ()

listenForMessage :: (IORef RoutingTable) -> IO ()  -- this isn't right yet
listenForMessage t = do message <- getLine 
                        t'   <- readIORef t
                        recompute message t' 



recompute :: Message -> RoutingTable -> IO ()
recompute m@(MyDist v d) t =  do
                                changeTable <- newIORef False
                                if check4Connection v t
                                    then do
                                        (b,e) <- checkLocal v t
                                        if b
                                            then do
                                                return ()
                                            else do
                                                if compareDist d (unMaybe e)
                                                    then do
                                                    newTable <- updateDist e m t
                                                    writeIORef changeTable True
                                                    else do
                                                        return ()
                                else do 
                                    makeNewConnection
                                    changeTable = True
                                c <- readIORef changeTable
                                if c == True
                                    sendInitTable
                                return ()

unMaybe :: Maybe a -> a
unMaybe Just a = a
                        
check4Connection :: Node -> RoutingTable -> IO (Bool, Maybe Entry)
check4Connoection v (Table [])     = (False, Nothing) 
check4Connoection v (Table (e:ex)) do
                                     entry <- takeTMVar e
                                     if  v == (\n _ _ -> n) entry
                                         then do 
                                             return (True, entry) -- true means: the entry's node is an existing connection within the current routing table
                                         else do
                                             return check4Connection v (Table ex)


checkLocal :: Node -> RoutingTable -> Bool
check4Connoection v (Table (e:_)) | v == e    = True  -- true means: the given entry is about the local node
                                  | otherwise = False

compareDist :: Int -> Entry -> Bool
compareDist d (Entry _ localD _) | (d+1) < localD    = True
                                 | otherwise         = False

updateDist :: Entry Message -> RoutingTable -> RoutingTable
updateDist en (MyDist s n d) (Table (e:ex) = do newEntry <- Entry n (d+1) s
                                                local    <- e
                                                edge     <- updateEdge ex newEntry en
                                                let total =  local : edge
                                                return $ Table total

 
updateEdge :: [Entry] -> Entry -> Entry -> [Entry]
updateEdge [] newE en     = newE
updateEdge (e:ex) newE en | e == en   = newE : ex
                          | otherwise = e : (updateEdge ex newE en)

removeEntry :: IORef RoutingTable -> Int -> IO ()
removeEntry t port =  shutdown p ShutdownBoth
                      putStrLn "Disconnecting with " ++ show p ++ "..." 
                      --dan hier nog functies om het uit de routing table te halen
-}

removeEntry :: IORef RoutingTable -> MVar [Node] -> Int -> IO ()
removeEntry t n port = do 
    nodes <- takeMVar n
    let nodes' = filter (\(Node x _ _) -> x /= port) nodes
    putMVar n nodes'
    -- remove from routing table?

addEntry :: IORef RoutingTable -> MVar [Node] -> Int -> IO ()
addEntry t n port = do
    -- create the node
    (node:_) <- createNodes [port]

    -- create routing table entry
    newEntry <- atomically $ newTMVar (Entry node 1 node)

    -- add node to node list
    nodes <- takeMVar n
    putMVar n (node : nodes)

    -- add entry to the table
    (Table e) <- readIORef t
    writeIORef t $ Table (newEntry : e)
