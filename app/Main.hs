module Main where

import Lib
import Control.Monad
import Control.Applicative
import Control.Concurrent as Concurrent
import Data.Maybe (fromMaybe)
import Data.ByteString.Base16 (decode)
import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Time.Clock as T
import Parser (evalParser, readByte, stop, ParserState)
import qualified GHC.IO.Handle as H
import qualified System.Environment as SysEnv
import qualified System.Process as SysProc
import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Widgets.Core as BC
import qualified Brick.Widgets.Border as BWB
import qualified Brick.Widgets.List as BWL
import qualified Brick.Widgets.Center as BWC
import qualified Brick.BChan as BChan
import qualified Graphics.Vty as V
import Brick.AttrMap (attrMap)
import qualified Data.Vector as Vec

type Parser = (Direction, ParserState WinDbgMsg ByteString)
data WinDbgMsg = WinDbgMsgNull deriving Show

data ChannelStats = ChannelStats { stats_in :: Integer,
                                   stats_out :: Integer,
                                   last_msg_time :: T.UTCTime}
data Direction = Inbound | Outbound deriving Eq
data ChannelMessage = ChannelMessage Direction ByteString deriving Show

data ChannelArgs = ChannelArgs {original :: String, duplicate :: String}
type SocatMsg = (Direction, String)

instance Show Direction where
  show Inbound = "in"
  show Outbound = "out"

main :: IO ()
main = do
  args <- readArgs
  channel <- prepareChannel args
  displayActivity channel

readArgs = do
  [pipe] <- SysEnv.getArgs
  return $ ChannelArgs {original = pipe, duplicate = duplicatePipeName pipe}

duplicatePipeName pipeName = pipeName ++ ".dup"

prepareChannel args = do
  (_, _, Just hOut, _) <- SysProc.createProcess socat{SysProc.std_err = SysProc.CreatePipe}
  chan <- BChan.newBChan 100
  Concurrent.forkIO $ readPipe hOut chan
  return chan
  where
    socatPipe = (++) "PIPE:"
    [originalPipe, duplicatePipe] = map (socatPipe . ($ args)) [original, duplicate]
    socat = SysProc.proc "socat" ["-x", originalPipe, duplicatePipe]

readPipe pipe chan = forever $ readPipeMsg pipe >>= writeChanMsg chan

readPipeMsg pipe = do
  (direction, _) <- fmap parseMsg $ H.hGetLine pipe
  fmap ((,) direction) $ H.hGetLine pipe


parseMsg (direction:' ':payload) = (parseDirection direction, payload)

parseDirection '>' = Inbound
parseDirection '<' = Outbound

writeChanMsg :: BChan.BChan SocatMsg -> SocatMsg -> IO ()
writeChanMsg = BChan.writeBChan

uiApp = BM.App {BM.appDraw = drawUI,
                BM.appChooseCursor = BM.showFirstCursor,
                BM.appHandleEvent = uiAppEvent,
                BM.appStartEvent = return,
                BM.appAttrMap = const $ attrMap V.defAttr []}

drawUI list = [ui]
  where
    label = BC.str "potok"
    box = BWB.borderWithLabel label $
               BC.hLimit 25 $
               BC.vLimit 15 $
               BWL.renderList listDrawElement True list
    ui = BWC.vCenter $ BC.vBox [BWC.hCenter box]

listDrawElement _sel msg = BC.str $ show msg

uiAppEvent msgs event =
  case event of
   BT.AppEvent msg ->
     let first = 0
     in BM.continue $ BWL.listInsert first msg msgs
   BT.VtyEvent (V.EvKey (V.KChar 'q') []) ->
     BM.halt msgs
   BT.VtyEvent ev ->
     BM.continue =<< BWL.handleListEvent ev msgs
   _ ->
     BM.continue msgs

displayActivity chan =
  void $ BM.customMain (V.mkVty V.defaultConfig) (Just chan) uiApp initialState
  where
    initialState = BWL.list () Vec.empty 1

parsingMain = newStats >>= readAndReport newParsers

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
