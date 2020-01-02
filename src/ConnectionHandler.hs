module ConnectionHandler where

import Data.IORef
import Network.Socket
import System.IO
import Control.Concurrent

import Models
import CommandHandler

listenForConnections :: (IORef RoutingTable) -> Socket -> IO ()
listenForConnections t serverSocket = do
  (connection, _) <- accept serverSocket
  _               <- forkIO $ handleConnection t connection
  listenForConnections t serverSocket

handleConnection :: (IORef RoutingTable) -> Socket -> IO ()
handleConnection t connection =
  do chandle <- socketToHandle connection ReadWriteMode
     handleConnection' chandle
  where handleConnection' handle = 
          do msg <- hGetLine handle
             t'  <- readIORef t
             handleCommand t' $ parseCommand msg
             handleConnection' handle