module Main where

import Network
import System.IO
import Message
import Text.Printf

nickMsg  = Message {origin = OriginDefault, operation = CmdNick, parameters = Just (ParamDefault ["peatBot"])}
userMsg  = Message {origin = OriginDefault, operation = CmdUser, parameters = Just (ParamUser {uname = "peatbot", mode = 0, realname = "peats bot"})}
joinMsg  = Message {origin = OriginDefault, operation = CmdJoin, parameters = Just (ParamChan {channels = [GlobalChannel "tanuki-test"], keys = Nothing})}
helloMsg = Message {origin = OriginDefault, operation = CmdPrivmsg, parameters = Just (ParamMsg {targets = [GlobalChannel "tanuki-test"], message = "hello, I am Peat's Haskell Bot!"})}

connect :: String -> Int -> IO ()
connect server port = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering   h NoBuffering
  hSetEncoding    h utf8
  hSetNewlineMode h NewlineMode { outputNL = CRLF, inputNL = CRLF } 

  -- set nick, user, join, and say hello!
  sendMessage h nickMsg
  sendMessage h userMsg
  sendMessage h joinMsg
  sendMessage h helloMsg

  forever $ do
    l <- hGetLine h  -- determines a line by NewlineMode (CRLF)
    print $ "> " ++ l
    case stringToErrorOrMessage l of
      Left err -> print $ show err
      Right msg -> handleMsg h msg
  where
    forever a = do a; forever a
    handleMsg h (Message origin command params) = do
      processCommand h command origin params


sendMessage :: Handle -> Message -> IO ()
sendMessage h m = case messageToErrorOrString m of
  Right msg -> do 
    hPrintf h "%s\r\n" msg -- shouldn't this be taken care of by the NewlineMode? hPrint h msg fails.
    print $ "< " ++ msg
  Left err  -> print err


-- takes a handle and message components, and throws it out to separate functions for handling different commands
processCommand :: Handle -> Operation -> Origin -> Maybe Parameters -> IO ()

-- CmdPing
processCommand h CmdPing origin param = do
  sendMessage h pong
  where
    pong = Message { origin = OriginDefault, operation = CmdPong, parameters = param }

-- CmdPrivmsg
processCommand h CmdPrivmsg origin param = do
  sendMessage h echo
  where
    echo = Message { origin = OriginDefault, operation = CmdPrivmsg, parameters = param }

-- CATCH ALL
processCommand h op org param = do
  print $ "No processCommand for " ++ (show op)


main = connect "irc.freenode.net" 6667
