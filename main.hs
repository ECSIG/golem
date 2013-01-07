--------------------
-- DATA THE ROBOT --
--------------------

import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Control.Monad.Reader
import Control.Exception
import System.Time
import Prelude hiding (catch)
 
server = "ecsig.com"
port   = 6667
chan   = "#ecsig"
nick   = "data"

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

----------------------------
------------ IO ------------
----------------------------

io :: IO a -> Net a
io = liftIO

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
       if ping s then pong s else parse (clean s)
  where
       forever a = a >> forever a
       clean     = drop 1 . dropWhile (/= ':') . drop 1
       ping x    = "PING :" `isPrefixOf` x
       pong x    = write "PONG" (':' : drop 6 x)

parse :: String -> Net ()
parse "" = return ()
parse text | nick `isPrefixOf` text = eval $ drop 1 $ words text
           | '!' == head text = eval $ words $ drop 1 text
parse _ = return ()

eval ::  [String] -> Net ()
eval [] = return ()
eval text = let cmd  = head text
                args = tail text
                act  = lookup cmd actions
            in case act of 
                 Nothing -> return ()
                 Just action -> action args

---------------------------
--------- Actions ---------
---------------------------

-- Actions table
actions :: [(String, [String] -> Net ())]
actions = [ ("quit", quit)
          , ("uptime", uptime)
          , ("echo", echo)
          ]

-- Exit network
quit :: [String] -> Net ()
quit notice = write "QUIT" (':' : unwords notice) 
              >> io (exitWith ExitSuccess)

-- Echo text
echo :: [String] -> Net ()
echo text = privmsg $ unwords text

-- Output uptime
uptime :: [String] -> Net ()
uptime _ = do
         now <- io getClockTime
         zero <- asks starttime
         privmsg $ pretty $ diffClockTimes now zero


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