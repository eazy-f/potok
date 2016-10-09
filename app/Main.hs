module Main where

import Lib
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe)

main :: IO ()
main = readAndReport $ newStats
     
readAndReport stats = do
  displayStats stats
  channelMessage <- readChannelMessage
  readAndReport $ updateStats channelMessage stats

data ChannelStats = ChannelStats { stats_in :: Integer, stats_out :: Integer }
data Direction = Inbound | Outbound deriving Show
data ChannelMessage = ChannelMessage Direction deriving Show

newStats = ChannelStats {stats_in = 0, stats_out = 0}

readChannelMessage :: IO ChannelMessage
readChannelMessage = liftM2 composeMessage readMessagePrologue readMessagePayload

readMessagePrologue = maybeReadMessagePrologue >>= (maybe readMessagePrologue return)

maybeReadMessagePrologue = messageDirection <$> getLine

messageDirection ('>':_) = Just Inbound
messageDirection ('<':_) = Just Outbound
messageDirection _ = Nothing

readMessagePayload = getLine

composeMessage direction _ = ChannelMessage direction

displayStats stats = sequence_ $ print <$> ([stats_in, stats_out] <*> pure stats)

updateStats (ChannelMessage Inbound) stats =
  stats {stats_in = (stats_in stats) + 1}

updateStats (ChannelMessage Outbound) stats =
  stats {stats_out = (stats_out stats) + 1}
  
