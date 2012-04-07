module Main where

import Network
import System.IO
import Message
import Text.Printf

nickMsg  = Message {origin = OriginDefault, operation = CmdNick, parameters = Just (ParamDefault ["peatBot"])}
userMsg  = Message {origin = OriginDefault, operation = CmdUser, parameters = Just (ParamUser {uname = "peatbot", mode = 0, realname = "peats bot"})}
joinMsg  = Message {origin = OriginDefault, operation = CmdJoin, parameters = Just (ParamChan {channels = [GlobalChannel "tanuki"], keys = Nothing})}
helloMsg = Message {origin = OriginDefault, operation = CmdPrivmsg, parameters = Just (ParamMsg {targets = [GlobalChannel "tanuki"], message = "hello, I am Peat's Haskell Bot!"})}

connect :: String -> Int -> IO ()
connect server port = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering

  -- set nick, user, join, and say hello!
  sendMessage h nickMsg
  sendMessage h userMsg
  sendMessage h joinMsg
  sendMessage h helloMsg

  forever $ do
    l <- hGetLine h
    print (">> " ++ l)
    case stringToErrorOrMessage l of
      Left err -> print $ show err
      Right msg -> handleMessage msg
  where
    forever a = do a; forever a

handleMessage :: Message -> IO ()
handleMessage (Message origin operation params) = do
  print $ (show operation) ++ " " ++ (show params)

sendMessage :: Handle -> Message -> IO ()
sendMessage h m = case messageToErrorOrString m of
  Right msg -> hPrintf h "%s\r\n" msg
  Left err  -> print err

main = do
  connect "irc.freenode.net" 6667
