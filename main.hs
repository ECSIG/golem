-- File: tutbot.hs

import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Control.Monad.Reader
import Control.Exception
import System.Time
import Prelude hiding (catch)
 
server = "irc.ecsig.com"
port   = 6667
chan   = "#ecsig"
nick   = "fubot"

type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = runReaderT run st


-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = bracket_
           (printf "Connecting to %s ... " server >> hFlush stdout)
           (putStrLn "done.")
           a

-- We're in the Net monad now, so we've connect successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :tutorial bot")
    write "JOIN" chan
    asks socket >>= listen

--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
      h <- asks socket
      io $ hPrintf h "%s %s\r\n" s t
      io $ printf    "> %s %s\n" s t

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

listen :: Handle -> Net ()
listen h = forever $ do
       s <- init `fmap` io (hGetLine h)
       io (putStrLn s)
       if ping s then pong s else eval (clean s)
  where
       forever a = a >> forever a
       clean     = drop 1 . dropWhile (/= ':') . drop 1
       ping x    = "PING :" `isPrefixOf` x
       pong x    = write "PONG" (':' : drop 6 x)

eval ::  String -> Net ()
eval  "!quit"                 = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval  "!uptime"               = uptime >>= privmsg
eval x | "!id" `isPrefixOf` x = privmsg (drop 4 x)
eval    _                     = return () -- ignore everything else

io :: IO a -> Net a
io = liftIO

uptime :: Net String
uptime = do
       now <- io getClockTime
       zero <- asks starttime
       return . pretty $ diffClockTimes now zero

--
-- Pretty print the date in '56d 7h 8m 47s' format
--
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