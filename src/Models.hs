module Models where

import Control.Concurrent.STM
import System.IO

data Entry = Entry Client Int Client -- dest, cost, closest neigh
data RoutingTable = Table [TMVar Entry]
data Client = Client Int Handle | LocalClient Int

data Command = Show 
             | Send Int String 
             | Make Int
             | Disconnect Int
             | Distance Int Int Int -- distance from neightbour, value, to me
             | Unknown 
          deriving Show

instance Show Client where
  show (LocalClient x) = show x
  show (Client x _)    = show x

instance Show Entry where
  show (Entry (LocalClient x) v (LocalClient _))
    = (show x) ++ " " ++ (show v) ++ " local"
  show (Entry client1 v client2) 
    = (show client1) ++ " " ++ (show v) ++ " " ++ (show client2)