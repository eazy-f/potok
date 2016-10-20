module Main where

import Lib
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.ByteString.Base16 (decode)
import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Time.Clock as T
import Parser (evalParser, readByte, stop, ParserState)

type Parser = (Direction, ParserState WinDbgMsg ByteString)
data WinDbgMsg = WinDbgMsgNull deriving Show


main :: IO ()
main = newStats >>= readAndReport newParsers

emptyState = stop WinDbgMsgNull mempty

newParsers = [(Inbound, emptyState),
              (Outbound, emptyState)]

readAndReport parsers stats = do
  time <- T.getCurrentTime
  newStats <- reportStats time stats
  channelMessage <- readChannelMessage
  let (amendedParsers, parsers') = feedMessage channelMessage parsers
  reportParsers amendedParsers
  readAndReport parsers' $ updateStats channelMessage newStats

data ChannelStats = ChannelStats { stats_in :: Integer,
                                   stats_out :: Integer,
                                   last_msg_time :: T.UTCTime}
data Direction = Inbound | Outbound deriving Eq
data ChannelMessage = ChannelMessage Direction ByteString deriving Show

instance Show Direction where
  show Inbound = "in"
  show Outbound = "out"

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

feedMessage msg parsers = foldl update ([], []) parsers
  where
    update = updateParsers msg

parseWinDbgNull '\0' = stop WinDbgMsgNull
parseWinDbgNull byte = parseWinDbgMsg

parseWinDbgMsg msg =
  readByte msg parseWinDbgNull

runWinDbgParser msg state = evalParser msg parseWinDbgMsg state

updateParsers msg (updated, all) parser@(parserDirection, parserState) = (updated', (parser':all))
  where
    (updated', parser') = case msgDirection of
                           dir | dir == parserDirection ->
                             let amended = (parserDirection, runWinDbgParser msgData parserState) in
                             ((amended:updated), amended)
                           otherwise ->
                             (updated, parser)
    ChannelMessage msgDirection msgData = msg

reportParsers = sequence . (map $ \parser -> (either ((printMsg $ fst parser) . fst) (\_ -> return ())) . snd $ parser)

printMsg direction msg = print $ show direction `mappend` ": " `mappend` show msg
