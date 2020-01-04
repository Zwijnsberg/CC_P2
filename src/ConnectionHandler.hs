module ConnectionHandler where

import Data.IORef
import Network.Socket
import System.IO
import Control.Concurrent

import Models
import CommandHandler

listenForConnections :: (IORef RoutingTable) -> MVar [Node] -> Socket -> IO ()
listenForConnections t n serverSocket = do
  (connection, _) <- accept serverSocket
  _               <- forkIO $ handleConnection t n connection
  listenForConnections t n serverSocket

handleConnection :: (IORef RoutingTable) -> MVar [Node] -> Socket -> IO ()
handleConnection t n connection =
  do chandle <- socketToHandle connection ReadWriteMode
     handleConnection' chandle
  where handleConnection' handle = 
          do msg <- hGetLine handle
             handleCommand t n $ parseCommand msg
             handleConnection' handle