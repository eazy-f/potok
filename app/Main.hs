module Main where

import Lib
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.ByteString.Base16 (decode)
import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Time.Clock as T

main :: IO ()
main = newStats >>= readAndReport
     
readAndReport stats = do
  time <- T.getCurrentTime
  newStats <- reportStats time stats
  channelMessage <- readChannelMessage
  readAndReport $ updateStats channelMessage newStats

data ChannelStats = ChannelStats { stats_in :: Integer,
                                   stats_out :: Integer,
                                   last_msg_time :: T.UTCTime}
data Direction = Inbound | Outbound deriving Show
data ChannelMessage = ChannelMessage Direction ByteString deriving Show

newStats = do
  now <- T.getCurrentTime
  return ChannelStats {stats_in = 0, stats_out = 0, last_msg_time = now}

readChannelMessage :: IO ChannelMessage
readChannelMessage = liftM2 composeMessage readMessagePrologue readMessagePayload

readMessagePrologue = maybeReadMessagePrologue >>= (maybe readMessagePrologue return)

maybeReadMessagePrologue = messageDirection <$> getLine

messageDirection ('>':_) = Just Inbound
messageDirection ('<':_) = Just Outbound
messageDirection _ = Nothing

readMessagePayload = fst . decode . pack . filter ((/=) ' ') <$> getLine

composeMessage direction payload = ChannelMessage direction payload

reportStats time stats | T.diffUTCTime time (last_msg_time stats) > 5 = 
                          printStats stats >> (return $ updateTime time stats)
reportStats _ stats = return stats

printStats stats = sequence_ $ print <$> ([stats_in, stats_out] <*> pure stats)

updateStats (ChannelMessage Inbound payload) stats =
  stats {stats_in = (stats_in stats) + (toInteger $ B.length payload)}

updateStats (ChannelMessage Outbound payload) stats =
  stats {stats_out = (stats_out stats) + (toInteger $ B.length payload)}
  
updateTime time stats = stats {last_msg_time = time}
