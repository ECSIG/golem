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
import Prelude hiding (catch)
import Control.Concurrent (threadDelay)
import Network.IRC.Commands(Channel)
import Network.IRC.Base(ServerName,UserName)
 
defaultName   = "golem-bot"
defaultChannel  = "#golem"
defaultServer = "localhost"
port   = 6667

type Net = ReaderT Bot IO

data Bot = Bot { startTime :: ClockTime
               , socket :: Handle
               , nickname :: UserName
               , channel :: Channel
               , server :: ServerName
               }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop       = runReaderT run

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
      args <- getArgs
      buildState defaultName defaultChannel defaultServer args
    where buildState n c s [] = do 
                     t <- getClockTime
                     h <- connectTo s (PortNumber (fromIntegral port))
                     hSetBuffering h NoBuffering
                     return (Bot t h n c s)
          buildState n c s (a:as)
                   | "nick=" `isPrefixOf` a
                       = buildState (drop 5 a) c s as
                   | "channel=" `isPrefixOf` a
                       = buildState n (drop 8 a) s as
                   | "server=" `isPrefixOf` a
                       = buildState n c (drop 7 a) as
          notify = bracket_
                   (print "Connecting to server ... " >> hFlush stdout)
                   (putStrLn "done.")

-- We're in the Net monad now, so we've connect successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  name <- asks nickname
  write "NICK" name
  write "USER" (name ++ "-bot 0 * :some kinda robot")
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
privmsg s = do 
  chan <- asks channel
  write "PRIVMSG" (chan ++ " :" ++ s)

listen :: Handle -> Net ()
listen h = forever $ do
       s <- init `fmap` io (hGetLine h)
       io (putStrLn s)
       if ping s 
       then pong s 
       else if welcome s
            then do 
              c <- asks channel
              joinChan [c]
            else parse (clean s)
    where
       clean     = drop 1 . dropWhile (/= ':') . drop 1
       ping x    = "PING :" `isPrefixOf` x
       pong x    = write "PONG" (':' : drop 6 x)
       welcome x = words x !! 1 == "001"

parse :: String -> Net ()
parse text | null text = return ()
--         | name `isPrefixOf` text = eval $ drop 1 $ words text
           | '!' == head text = eval $ words $ drop 1 text
           | otherwise = return ()

eval ::  [String] -> Net ()
eval text | null text = return ()
          | otherwise = 
              let cmd  = head text
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
              >> io exitSuccess

-- Echo text
echo :: [String] -> Net ()
echo text = privmsg $ unwords text

-- Join a channel
joinChan :: [String] -> Net ()
joinChan chan = write "JOIN" (':' : unwords chan)

-- Output uptime
uptime :: [String] -> Net ()
uptime _ = do
         now <- io getClockTime
         zero <- asks startTime
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

