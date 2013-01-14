--------------------
-- GOLEM THE ROBOT --
--------------------

import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import System.Environment (getArgs)
import Control.Monad.Reader
import Control.Exception
import System.Time
import Control.Applicative((<$>))
import Prelude hiding (catch)
import Control.Concurrent (threadDelay)
import qualified Network.IRC.Commands as IrcC
import qualified Network.IRC.Base as IrcB
import qualified Network.IRC.Parser as IrcP
 
name = "golem"
chan = "#golem"
serv = "irc.ecsig.com"
port = 6667

type Net = ReaderT Bot IO

data Bot = Bot { startTime :: ClockTime
               , socket :: Handle
               , nickname :: IrcB.UserName
               , channel :: IrcC.Channel
               , server :: IrcB.ServerName
               }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop       = runReaderT run

-- builds a bot from defaults & provided args
connect :: IO Bot
connect = notify $ do
  args <- getArgs
  makeBot name chan serv args
    where makeBot n c s (a:as)
              | "nick=" `isPrefixOf` a
                  = makeBot (drop 5 a) c s as
              | "channel=" `isPrefixOf` a
                  = makeBot n (drop 8 a) s as
              | "server=" `isPrefixOf` a
                  = makeBot n c (drop 7 a) as
          makeBot n c s [] = do
            t <- getClockTime
            h <- connectTo s (PortNumber (fromIntegral port))
            hSetBuffering h NoBuffering
            return Bot { startTime=t 
                       , socket=h 
                       , nickname=n 
                       , channel=c 
                       , server=s }
          notify = bracket_
                   (print "Connecting to server ... " >> hFlush stdout)
                   (putStrLn "done.")

-- We're in the Net monad now, so we've connect successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  name <- asks nickname
  write $ IrcC.nick name
  write $ IrcC.user (take 6 name ++ "-bot") "0" "*" "some kinda robot"
  asks socket >>= listen

----------------------------
------------ IO ------------
----------------------------

io :: IO a -> Net a
io = liftIO

-- prints message to stdout & network
write :: IrcB.Message -> Net ()
write msg = do
  h <- asks socket
  io $ hPutStrLn h str
  io $ putStrLn str
    where str = IrcB.encode msg

-- waits for message, decodes & passes to parser, waits again
listen :: Handle -> Net ()
listen h = forever $ do
       msg <- IrcP.decode <$> io (hGetLine h)
       case msg of 
         Nothing -> return ()
         Just m -> io (putStrLn $ IrcB.encode m) >> parse m

-- determines what to do with message based on command field
parse :: IrcB.Message -> Net ()
parse (IrcB.Message pre cmd params) =
    case cmd of
      "PING" -> write $ IrcB.Message pre "PONG" params
      "001" -> asks channel >>= joinChan
      "PRIVMSG" -> readMsg pre params
      _ -> return ()

-- checks if incoming private messages start with ! or botname
readMsg :: Maybe IrcB.Prefix -> [IrcB.Parameter] -> Net ()
readMsg (Just nick) [chan,text] =
        let (first:rest) = words text in
        do botName <- asks nickname
           case first of
             ('!':cmd) -> eval nick (cmd:rest) chan
             botName -> eval nick rest chan

-- evaluates messages that appear to be intended for bot
eval :: IrcB.Prefix -> [IrcB.Parameter] -> IrcC.Channel -> Net ()
eval nick (cmd:args) chan =
    case lookup cmd actions of
      Nothing -> return ()
      Just action -> action nick args chan
eval _ _ _ = return ()

---------------------------
--------- Actions ---------
---------------------------

-- Actions table
actions :: [(String, IrcB.Prefix -> [IrcB.Parameter] -> IrcC.Channel -> Net ())]
actions = [ ("quit", quit)
          , ("uptime", uptime)
          , ("echo", echo)
          , ("swap", swap)
          , ("join", \_ c _ -> joinChan $ unwords c)
          , ("part", \_ c _ -> partChan $ unwords c)
          ]

-- changes nickname
swap :: IrcB.Prefix -> [IrcB.UserName] -> IrcC.Channel -> Net ()
swap _ [name] _  = do
  write $ IrcC.nick name
  Bot t h _ c s <- ask
  local (return $ Bot t h name c s) $ listen h

-- Exit network
quit :: IrcB.Prefix ->[IrcB.Parameter] -> IrcC.Channel ->  Net ()
quit _ _ notice = do
  write $ IrcC.quit $ (\n -> if null n then Nothing else Just n) notice
  io exitSuccess

-- Echo text
echo :: IrcB.Prefix -> [IrcB.Parameter] -> IrcC.Channel -> Net ()
echo n = privmsg n . unwords

-- sends private message
privmsg :: IrcB.Prefix -> String -> IrcC.Channel -> Net ()
privmsg (IrcB.NickName n _ _) s c = do 
  botName <- asks nickname
  let c' = if c==botName then n else c
  write $ IrcC.privmsg c' s

-- Join a channel
joinChan :: IrcB.Parameter -> Net ()
joinChan chan | null chan = return ()
              | otherwise = write $ IrcC.joinChan chan

-- parts from channel
partChan :: IrcB.Parameter -> Net ()
partChan chan | null chan = return ()
              | otherwise = write $ IrcC.part chan

-- Output uptime
uptime :: IrcB.Prefix -> [IrcB.Parameter] -> IrcC.Channel -> Net ()
uptime n _ c = do
         now <- io getClockTime
         zero <- asks startTime
         privmsg n c $ pretty $ diffClockTimes now zero


-- Pretty print the date in '56d 7h 8m 47s' format
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s

