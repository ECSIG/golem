import Network.IRC
import System.Exit(exitSuccess)
import Control.Exception(bracket)
import Control.Applicative((<$>))
import Control.Monad(forever)
import Control.Monad.Reader(local,ask,asks,ReaderT,runReaderT,liftIO)
import Data.Char(isSpace)
import Data.List(isPrefixOf)
import Network(PortID(PortNumber),connectTo)
import System.IO(Handle,hClose,hGetLine,hPutStrLn,hSetBuffering,BufferMode(NoBuffering))

defaultName = "golbat"
defaultChan = "#golem"
defaultServ = "irc.ecsig.com"
defaultPort = PortNumber 6667
defaultActs = [("quit", quitAct),
              ("echo", echoAct),
              ("join", joinAct),
              ("part", partAct),
              ("nick", nickAct)]

data IrcData = IrcData
    { handle   :: Handle
    , userName :: UserName
    , actions  :: ActionList
    }

type IrcState = ReaderT IrcData IO

type ActionList = [(String, Request -> IrcState Action)]

data Action = Join Channel
            | Part Channel
            | Nick UserName
            | Echo Channel String
            | Quit String
            | Pong String
            | Idle

data Request = None
             | Welcome
             | Ping String
             | UserReq
               { reqUser :: String
               , reqChan :: String
               , reqKey  :: String
               , reqArgs :: [String]
               }

io :: IO a -> IrcState a
io = liftIO

main :: IO ()
main = bracket connect disconnect loop
    where
      disconnect = hClose . handle
      loop       = runReaderT run

-- initialize connection
connect :: IO IrcData
connect = do
  handle <- connectTo defaultServ defaultPort
  hSetBuffering handle NoBuffering
  return $ IrcData handle defaultName defaultActs

-- inform server of identity, then listen
run :: IrcState ()
run = do
  name <- asks userName
  write $ nick name
  write $ user (take 6 name ++ "-bot") "0" "*" "GOLEM XIV"
  asks handle >>= listen

-- waits for message, decodes & passes to parser, waits again
listen :: Handle -> IrcState ()
listen h = forever $ do
  msg <- decode <$> io (hGetLine h)
  case msg of
    Nothing -> return ()
    Just m -> do 
             io (putStrLn $ encode m)
             readMsg m >>= consider >>= respond

-- searches message for potential request
readMsg :: Message -> IrcState Request
readMsg (Message prefix command params) =
    case command of
      "PRIVMSG" -> readPriv prefix (head params) (head $ tail params)
      "PING"    -> return $ Ping $ unwords params
      "001"     -> return Welcome
      _         -> return None

-- searches private message for user request
readPriv :: Maybe Prefix -> Channel -> String -> IrcState Request
readPriv (Just (NickName name _ _)) chan stmt = do
  botName <- asks userName
  let first:rest = words stmt
  return
    (if      first==botName 
     then    UserReq name chan (head rest) (tail rest)
     else if head first=='!'
     then    UserReq name chan (tail first) rest
     else    None)

-- determines appropriate action for given request
consider :: Request -> IrcState Action
consider None = return Idle
consider (Ping p) = return $ Pong p
consider Welcome = return $ Join defaultChan
consider req@(UserReq name chan key args) = do
   actionList <- asks actions
   case lookup key actionList of
     Nothing -> return Idle
     Just action -> action req

-- carries out some action
respond :: Action -> IrcState ()
respond (Echo chan text) = write $ privmsg chan text
respond (Pong text) = write $ Message Nothing "PONG" [text]
respond (Join chan) = write $ joinChan chan
respond (Part chan) = write $ part chan
respond (Nick name) = do
                      IrcData h _ a <- ask
                      local (return $ IrcData h name a) $ listen h
                      write $ nick name
respond (Quit msg) | null msg  = do
                                 write $ quit Nothing
                                 io exitSuccess
                   | otherwise = do
                                 write $ quit $ Just msg
                                 io exitSuccess
respond Idle = return ()

-- prints message to stdout & network
write :: Message -> IrcState ()
write msg = do
  h <- asks handle
  io $ hPutStrLn h s
  io $ putStrLn s
    where s = encode msg

-- change nickname
nickAct :: Request -> IrcState Action
nickAct (UserReq _ _ _ name) | null name = return Idle
                             | otherwise = return $ Nick $ head name

-- Exit network
quitAct :: Request -> IrcState Action
quitAct (UserReq _ _ _ notice) = return $ Quit $ unwords notice

-- Echo text
echoAct :: Request -> IrcState Action
echoAct (UserReq _ chan _ text) = return $ Echo chan $ unwords text


-- Join a channel
joinAct :: Request -> IrcState Action
joinAct (UserReq _ _ _ chan) | null chan = return Idle
                             | otherwise = return $ Join $ head chan

-- parts from channel
partAct :: Request -> IrcState Action
partAct (UserReq _ _ _ chan) | null chan = return Idle
                             | otherwise = return $ Part $ head chan