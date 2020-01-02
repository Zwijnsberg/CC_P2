module Models where

import Control.Concurrent.STM
import System.IO

data Entry = Entry Node Int Node -- dest, cost, closest neigh
data RoutingTable = Table [TMVar Entry]
data Node = Node Int Handle | LocalNode Int

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