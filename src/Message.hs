{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Message where

{- Just getting started on an IRC protocol parser, to learn me a Haskell and a Parsec. -}

import Text.ParserCombinators.Parsec
import Data.Foldable (asum)
import Data.List (intercalate)
import Data.List.Split (splitOn)

{- data -}

data Message    = Message {
                    origin :: Origin,               -- "prefix" in the RFC
                    operation :: Operation,
                    parameters :: Maybe Parameters
                  }
                  deriving (Eq, Show)

data Origin     = OriginHost Hostname  -- server the message is from
                | OriginUser User      -- user the message is from
                | OriginDefault        -- the default is the attached server, for a client 
                deriving (Eq, Show)

data Hostname   = Hostname String
                  deriving (Eq, Show)

data User       = User {
                    nickname :: Maybe Nickname, 
                    username :: Maybe Username,
                    hostname :: Maybe Hostname,
                    server   :: Maybe Hostname
                  } deriving (Eq, Show)

data Nickname   = Nickname String
                  deriving (Eq, Show)

data Username   = Username String
                  deriving (Eq, Show)

data Operation  = CmdPass     -- connection commands
                | CmdNick
                | CmdUser
                | CmdOper
                | CmdMode
                | CmdService
                | CmdQuit
                | CmdSquit
                | CmdJoin     -- channel commands
                | CmdPart
                | CmdTopic
                | CmdNames
                | CmdList
                | CmdInvite
                | CmdKick
                | CmdPrivmsg  -- messaging commands
                | CmdNotice
                | CmdMotd     -- server commands
                | CmdLusers
                | CmdVersion
                | CmdStats
                | CmdLinks
                | CmdTime
                | CmdConnect
                | CmdTrace
                | CmdAdmin
                | CmdInfo
                | CmdServlist -- service commands
                | CmdSquery
                | CmdWho      -- user commands
                | CmdWhois
                | CmdWhowas
                | CmdKill     -- misc commands
                | CmdPing
                | CmdPong
                | CmdError
                | CmdAway     -- optional messages
                | CmdRehash
                | CmdDie
                | CmdRestart
                | CmdSummon
                | CmdUsers
                | CmdWallops
                | CmdUserhost
                | CmdIson
                | OpUnknown String
                | RplWelcome  -- replies
                | RplYourHost
                | RplCreated
                | RplMyInfo
                | RplBounce
                | RplUserHost
                | RplIson
                | RplAway
                | RplUnAway
                | RplNowAway
                | RplWhoIsUser
                | RplWhoIsServer
                | RplWhoIsOperator
                | RplEndOfWhoIs
                | RplWhoIsChannels
                | RplWhoWasUser
                | RplEndOfWhoWas
                | RplList
                | RplListEnd
                | RplUniqOpIs
                | RplChannelModeIs
                | RplNoTopic
                | RplTopic
                | RplTopicWhoTime
                | RplInviting
                | RplSummoning
                | RplInviteList
                | RplEndOfInviteList
                | RplExceptList
                | RplEndOfExceptList
                | RplVersion
                | RplWhoReply
                | RplEndOfWho
                | RplNameReply
                | RplEndOfNames
                | RplLinks
                | RplBanList
                | RplEndOfBanList
                | RplInfo
                | RplEndOfInfo
                | RplMotdStart
                | RplMotd
                | RplEndOfMotd
                | RplYouAreOper
                | RplYouAreHashing
                | RplTime
                | RplUsers
                | RplEndOfUsers
                | RplNoUsers
                | RplTraceLink
                | RplTraceConnecting
                | RplTraceHandshake
                | RplTraceUnknown
                | RplTraceOperator
                | RplTraceUser
                | RplTraceServer
                | RplTraceService
                | RplTraceNewType
                | RplTraceClass
                | RplTraceLog
                | RplTraceEnd
                | RplStatsLinkInfo
                | RplStatsCommands
                | RplEndOfStats
                | RplStatsUptime
                | RplStatsOLine
                | RplUModeIs
                | RplServList
                | RplEndOfServList
                | RplStatsConnections
                | RplLUserClient
                | RplLUserOp
                | RplLUserUnknown
                | RplLUserChannels
                | RplLUserMe
                | RplAdminMe
                | RplAdminLoc1
                | RplAdminLoc2
                | RplAdminEmail
                | RplTryAgain
                | RplLocalUsers
                | RplGlobalUsers
                | ErrNoSuchNick   -- errors
                | ErrNoSuchServer
                | ErrNoSuchChannel
                | ErrCannotSendtoChannel
                | ErrTooManyChannels
                | ErrWasNoSuchNick
                | ErrTooManyTargets
                | ErrNoSuchService
                | ErrNoOrigin
                | ErrNoRecipient
                | ErrNoTextToSend
                | ErrNoTopLevel
                | ErrWildTopLevel
                | ErrBadMask
                | ErrUnknownCommand
                | ErrNoMotd
                | ErrAdminInfo
                | ErrFileError
                | ErrNoNicknameGiven
                | ErrErroneousNickname
                | ErrNicknameInUse
                | ErrNickCollision
                | ErrUnavailableResource
                | ErrUserNotInChannel
                | ErrNotInChannel
                | ErrUserOnChannel
                | ErrNoLogin
                | ErrSummonDisabled
                | ErrUsersDisabled
                | ErrNeedMoreParams
                | ErrAlreadyRegistered
                | ErrNoPermForHost
                | ErrPasswordMismatch
                | ErrYouAreBanned
                | ErrYouWillBeBanned
                | ErrKeySet
                | ErrChannelIsFull
                | ErrUnknownMode
                | ErrInviteOnlyMode
                | ErrBannedFromChannel
                | ErrBadChannelKey
                | ErrBadChannelMask
                | ErrNoChannelModes
                | ErrBanListFull
                | ErrNoPrivileges
                | ErrChannelOpNeeded
                | ErrCantKillServer
                | ErrRestricted
                | ErrNotChannelCreator
                | ErrNoOperHost
                | ErrUModeUnknownFlag
                | ErrUsersDontMatch
                deriving (Eq, Show)

data Parameters = ParamMsg { targets :: [Target], message :: String }
                | ParamChan { channels:: [Target], keys :: Maybe [String] }
                | ParamUser { uname :: String, mode :: Int, realname :: String }
                | ParamDefault [String]
                deriving (Eq, Show)

data Target     = GlobalChannel String
                | LocalChannel String
                | TargetUser User
                | TargetUnknown String
                deriving (Eq, Show)

{- associations for commands -}

operationLookup :: [(String, Operation)]
operationLookup = [ ("PASS"    , CmdPass)     -- connection commands
                  , ("NICK"    , CmdNick)
                  , ("USER"    , CmdUser)
                  , ("OPER"    , CmdOper)
                  , ("MODE"    , CmdMode)
                  , ("SERVICE" , CmdService)
                  , ("QUIT"    , CmdQuit)
                  , ("SQUIT"   , CmdSquit)
                  , ("JOIN"    , CmdJoin)     -- channel commands
                  , ("PART"    , CmdPart)
                  , ("TOPIC"   , CmdTopic)
                  , ("NAMES"   , CmdNames)
                  , ("LIST"    , CmdList)
                  , ("INVITE"  , CmdInvite)
                  , ("KICK"    , CmdKick)
                  , ("PRIVMSG" , CmdPrivmsg)  -- messaging commands
                  , ("NOTICE"  , CmdNotice)
                  , ("MOTD"    , CmdMotd)     -- server commands
                  , ("LUSERS"  , CmdLusers)
                  , ("VERSION" , CmdVersion)
                  , ("STATS"   , CmdStats)
                  , ("LINKS"   , CmdLinks)
                  , ("TIME"    , CmdTime)
                  , ("CONNECT" , CmdConnect)
                  , ("TRACE"   , CmdTrace)
                  , ("ADMIN"   , CmdAdmin)
                  , ("INFO"    , CmdInfo)
                  , ("SERVLIST", CmdServlist) -- service commands
                  , ("SQUERY"  , CmdSquery)
                  , ("WHO"     , CmdWho)      -- user commands
                  , ("WHOIS"   , CmdWhois)
                  , ("WHOWAS"  , CmdWhowas)
                  , ("KILL"    , CmdKill)     -- misc commands
                  , ("PING"    , CmdPing)
                  , ("PONG"    , CmdPong)
                  , ("ERROR"   , CmdError)
                  , ("AWAY"    , CmdAway)     -- optional messages
                  , ("REHASH"  , CmdRehash)
                  , ("DIE"     , CmdDie)
                  , ("RESTART" , CmdRestart)
                  , ("SUMMON"  , CmdSummon)
                  , ("USERS"   , CmdUsers)
                  , ("WALLOPS" , CmdWallops)
                  , ("USERHOST", CmdUserhost)
                  , ("ISON"    , CmdIson)
                  , ("001"     , RplWelcome)  -- replies
                  , ("002"     , RplYourHost)
                  , ("003"     , RplCreated)
                  , ("004"     , RplMyInfo)
                  , ("005"     , RplBounce)
                  , ("302"     , RplUserHost)
                  , ("303"     , RplIson)
                  , ("301"     , RplAway)
                  , ("305"     , RplUnAway)
                  , ("306"     , RplNowAway)
                  , ("311"     , RplWhoIsUser)
                  , ("312"     , RplWhoIsServer)
                  , ("313"     , RplWhoIsOperator)
                  , ("318"     , RplEndOfWhoIs)
                  , ("319"     , RplWhoIsChannels)
                  , ("314"     , RplWhoWasUser)
                  , ("369"     , RplEndOfWhoWas)
                  , ("322"     , RplList)
                  , ("323"     , RplListEnd)
                  , ("325"     , RplUniqOpIs)
                  , ("324"     , RplChannelModeIs)
                  , ("331"     , RplNoTopic)
                  , ("332"     , RplTopic)
                  , ("333"     , RplTopicWhoTime)
                  , ("341"     , RplInviting)
                  , ("342"     , RplSummoning)
                  , ("346"     , RplInviteList)
                  , ("347"     , RplEndOfInviteList)
                  , ("348"     , RplExceptList)
                  , ("349"     , RplEndOfExceptList)
                  , ("351"     , RplVersion)
                  , ("352"     , RplWhoReply)
                  , ("315"     , RplEndOfWho)
                  , ("353"     , RplNameReply)
                  , ("366"     , RplEndOfNames)
                  , ("364"     , RplLinks)
                  , ("367"     , RplBanList)
                  , ("368"     , RplEndOfBanList)
                  , ("371"     , RplInfo)
                  , ("374"     , RplEndOfInfo)
                  , ("375"     , RplMotdStart)
                  , ("372"     , RplMotd)
                  , ("376"     , RplEndOfMotd)
                  , ("381"     , RplYouAreOper)
                  , ("383"     , RplYouAreHashing)
                  , ("391"     , RplTime)
                  , ("393"     , RplUsers)
                  , ("394"     , RplEndOfUsers)
                  , ("395"     , RplNoUsers)
                  , ("200"     , RplTraceLink)
                  , ("201"     , RplTraceConnecting)
                  , ("202"     , RplTraceHandshake)
                  , ("203"     , RplTraceUnknown)
                  , ("204"     , RplTraceOperator)
                  , ("205"     , RplTraceUser)
                  , ("206"     , RplTraceServer)
                  , ("207"     , RplTraceService)
                  , ("208"     , RplTraceNewType)
                  , ("209"     , RplTraceClass)
                  , ("261"     , RplTraceLog)
                  , ("262"     , RplTraceEnd)
                  , ("211"     , RplStatsLinkInfo)
                  , ("212"     , RplStatsCommands)
                  , ("219"     , RplEndOfStats)
                  , ("242"     , RplStatsUptime)
                  , ("243"     , RplStatsOLine)
                  , ("221"     , RplUModeIs)
                  , ("234"     , RplServList)
                  , ("235"     , RplEndOfServList)
                  , ("250"     , RplStatsConnections)
                  , ("251"     , RplLUserClient)
                  , ("252"     , RplLUserOp)
                  , ("253"     , RplLUserUnknown)
                  , ("254"     , RplLUserChannels)
                  , ("255"     , RplLUserMe)
                  , ("265"     , RplLocalUsers)
                  , ("266"     , RplGlobalUsers)
                  , ("256"     , RplAdminMe)
                  , ("257"     , RplAdminLoc1)
                  , ("258"     , RplAdminLoc2)
                  , ("259"     , RplAdminEmail)
                  , ("263"     , RplTryAgain)
                  , ("401"     , ErrNoSuchNick)   -- errors
                  , ("402"     , ErrNoSuchServer)
                  , ("403"     , ErrNoSuchChannel)
                  , ("404"     , ErrCannotSendtoChannel)
                  , ("405"     , ErrTooManyChannels)
                  , ("406"     , ErrWasNoSuchNick)
                  , ("407"     , ErrTooManyTargets)
                  , ("408"     , ErrNoSuchService)
                  , ("409"     , ErrNoOrigin)
                  , ("411"     , ErrNoRecipient)
                  , ("412"     , ErrNoTextToSend)
                  , ("413"     , ErrNoTopLevel)
                  , ("414"     , ErrWildTopLevel)
                  , ("415"     , ErrBadMask)
                  , ("421"     , ErrUnknownCommand)
                  , ("422"     , ErrNoMotd)
                  , ("423"     , ErrAdminInfo)
                  , ("424"     , ErrFileError)
                  , ("431"     , ErrNoNicknameGiven)
                  , ("432"     , ErrErroneousNickname)
                  , ("433"     , ErrNicknameInUse)
                  , ("436"     , ErrNickCollision)
                  , ("437"     , ErrUnavailableResource)
                  , ("441"     , ErrUserNotInChannel)
                  , ("442"     , ErrNotInChannel)
                  , ("443"     , ErrUserOnChannel)
                  , ("444"     , ErrNoLogin)
                  , ("445"     , ErrSummonDisabled)
                  , ("446"     , ErrUsersDisabled)
                  , ("461"     , ErrNeedMoreParams)
                  , ("462"     , ErrAlreadyRegistered)
                  , ("463"     , ErrNoPermForHost)
                  , ("464"     , ErrPasswordMismatch)
                  , ("465"     , ErrYouAreBanned)
                  , ("466"     , ErrYouWillBeBanned)
                  , ("467"     , ErrKeySet)
                  , ("471"     , ErrChannelIsFull)
                  , ("472"     , ErrUnknownMode)
                  , ("473"     , ErrInviteOnlyMode)
                  , ("474"     , ErrBannedFromChannel)
                  , ("475"     , ErrBadChannelKey)
                  , ("476"     , ErrBadChannelMask)
                  , ("477"     , ErrNoChannelModes)
                  , ("478"     , ErrBanListFull)
                  , ("481"     , ErrNoPrivileges)
                  , ("482"     , ErrChannelOpNeeded)
                  , ("483"     , ErrCantKillServer)
                  , ("484"     , ErrRestricted)
                  , ("485"     , ErrNotChannelCreator)
                  , ("491"     , ErrNoOperHost)
                  , ("501"     , ErrUModeUnknownFlag)
                  , ("502"     , ErrUsersDontMatch)
                  ]

{- Main interface for pulling a message out of a String -}

stringToErrorOrMessage :: String -> Either ParseError Message
stringToErrorOrMessage s = parse parseMessage "Error parsing message" s


{- 
  Main interface for turning a message back into a String 

  This is really incomplete! Ignores origin! Danger, danger!
-}

messageToErrorOrString :: Message -> Either String String
messageToErrorOrString (Message origin operation params) = case (opString, paramString) of
    (Left err, _)      -> Left err
    (_, Left err)      -> Left err
    (Right o, Right p) -> Right (o ++ " " ++ p)
  where
    opString = generateOperation operation
    paramString = generateParameters params


generateOperation :: Operation -> Either String String
generateOperation c = case c of
  CmdNick    -> Right "NICK"
  CmdUser    -> Right "USER"
  CmdJoin    -> Right "JOIN"
  CmdPrivmsg -> Right "PRIVMSG"
  CmdPong    -> Right "PONG"
  _          -> Left ("Unknown command: " ++ (show c))


generateParameters :: Maybe Parameters -> Either String String
generateParameters p = case p of
    Nothing                -> Right ""
    Just (ParamUser u m r) -> Right (genUser u m r)
    Just (ParamDefault ps) -> Right (intercalate " :" ps)
    Just (ParamChan cs ks) -> Right (chansAndKeys  cs ks)
    Just (ParamMsg ts msg) -> Right (targetsAndMsg ts msg)
  where
    chansAndKeys cs ks = case (cs, ks) of
      (cs, Nothing) -> targMerge cs
      (cs, Just ks) -> (targMerge cs) ++ " " ++ (commaMerge ks) 
    targetsAndMsg ts msg = (targMerge ts) ++ " :" ++ msg
    commaMerge xs = intercalate "," xs
    targMerge cs = commaMerge $ map getTargs cs
    getTargs c = case c of
      LocalChannel  lc -> "&" ++ lc
      GlobalChannel gc -> "#" ++ gc
    genUser u m r = intercalate " " [u, (show m), "*",":" ++ r]

 
{- Parsers; top level -}

parseMessage :: Parser Message
parseMessage = do org <- option OriginDefault parseOrigin  -- OriginDefault if parseOrigin fails
                  op  <- parseOperation                    -- operation must always be present
                  p   <- optionMaybe (parseParameters op)  -- parameters are optional
                  return $ Message org op p


{- Parsers; Origin -}

parseOrigin :: Parser Origin 
parseOrigin = do char ':'
                 x <- (try pOriginUserNUH <|> try pOriginUserNH <|> try pOriginHost <|> try pOriginUserN)
                 char ' '
                 return x
              where
                 pOriginUserNUH = do u <- parseUserNUH
                                     return $ OriginUser u
                 pOriginUserNH = do u <- parseUserNH
                                    return $ OriginUser u
                 pOriginUserN = do u <- parseUserN
                                   return $ OriginUser u
                 pOriginHost = do h <- parseHostname
                                  return $ OriginHost h

{- origin user parsers -}

parseUserNUH :: Parser User
parseUserNUH = do nick <- parseNickname
                  char '!'
                  user <- parseUsername
                  char '@'
                  host <- parseHostname
                  return $ User (Just nick) (Just user) (Just host) Nothing


parseUserNH :: Parser User
parseUserNH = do nick <- parseNickname
                 char '@'
                 host <- parseHostname
                 return $ User (Just nick) Nothing (Just host) Nothing


parseUserN :: Parser User
parseUserN = do nick <- parseNickname
                return $ User (Just nick) Nothing Nothing Nothing


{- Parsers; Operation -}

parseOperation :: Parser Operation
parseOperation = asum opParsers
                 where
                   opParsers = map mkParser operationLookup
                   mkParser (str, op) = try (string str >> return op)


{- Parsers; parameters -}

parseParameters :: Operation -> Parser Parameters
parseParameters o = do char ' '
                       p <- parseParamsForOp o 
                       return p

{- Parameter parsers for specific operations -}

parseParamsForOp :: Operation -> Parser Parameters
parseParamsForOp CmdPrivmsg  = parseParamsForMsg
parseParamsForOp CmdNotice   = parseParamsForMsg
parseParamsForOp CmdJoin     = try parseParamsForChanAndKey <|> try parseParamsForChan
parseParamsForOp CmdUser     = parseParamsForUser
parseParamsForOp _           = parseParamsForDefault

parseParamsForUser :: Parser Parameters
parseParamsForUser = do u <- many (alphaNum)
                        char ' '
                        m <- digit
                        string " * :"
                        r <- many (alphaNum <|> char ' ') 
                        return $ ParamUser u (read [m] :: Int) r
                                             
parseParamsForMsg :: Parser Parameters
parseParamsForMsg = do ts <- parseTargets
                       string " :"
                       m <- many (noneOf "\r\n")
                       return $ ParamMsg ts m


parseParamsForDefault :: Parser Parameters
parseParamsForDefault = do s <- many (noneOf "\r\n")
                           return $ ParamDefault $ splitOn " :" s

parseParamsForChanAndKey :: Parser Parameters
parseParamsForChanAndKey = do chans <- parseTargets
                              char ' '
                              keys <- sepBy1 (many (noneOf ", ")) (char ',')
                              return $ ParamChan chans (Just keys)

parseParamsForChan :: Parser Parameters
parseParamsForChan = do chans <- parseTargets
                        return $ ParamChan chans Nothing


parseTargets :: Parser [Target]
parseTargets = sepBy1 parseTarget pSeparator
               where
                 pSeparator = char ','


parseTarget :: Parser Target -- still needs targetmask parser!
parseTarget = try pUnknown <|> try parseChannel <|> try pTargetUserUHS <|> try pTargetUserUH <|> try pTargetUserNUH <|> try pTargetUserN 
              where
                pTargetUserUHS = do u <- parseUserUHS
                                    return $ TargetUser u
                pTargetUserUH = do u <- parseUserUH
                                   return $ TargetUser u
                pTargetUserN = do u <- parseUserN
                                  return $ TargetUser u
                pTargetUserNUH = do u <- parseUserNUH
                                    return $ TargetUser u
                pUnknown = do c <- char '*' 
                              return $ TargetUnknown [c]


parseChannel :: Parser Target
parseChannel = try pChanL <|> try pChanG
               where
                 pChanL = do char '&'
                             n <- pChanName
                             return $ LocalChannel n
                 pChanG = do char '#'
                             n <- pChanName
                             return $ GlobalChannel n
                 pChanName = many (alphaNum <|> oneOf ":-")


{- msgto user parsers -}

parseUserUHS :: Parser User
parseUserUHS = do user <- parseUsername
                  char '%'
                  host <- parseHostname
                  char '@'
                  serv <- parseHostname
                  return $ User Nothing (Just user) (Just host) (Just serv)

parseUserUH :: Parser User
parseUserUH = do user <- parseUsername
                 char '%'
                 host <- parseHostname
                 return $ User Nothing (Just user) (Just host) Nothing


{- Parsers; basic structures -}

parseHostname :: Parser Hostname
parseHostname = do segments <- (sepBy1 pSegment pSeparator)
                   return $ Hostname $ intercalate "." segments
                where
                   pSegment   = do segment <- many1 (alphaNum <|> char '-')
                                   if last segment == '-'
                                     then fail "domain segment must not end with hyphen"
                                     else return $ segment
                   pSeparator = char '.'


parseNickname :: Parser Nickname
parseNickname = do x <- many (alphaNum <|> (oneOf "-"))
                   return $ Nickname x

parseUsername :: Parser Username
parseUsername = do x <- many (alphaNum <|> (oneOf ".-~+"))
                   return $ Username x


{-
Try me:

  $ ghci

  Prelude> :l Main.hs

  *Main> parse parseMessage "" ":peatBot!peat@peat.org PRIVMSG &local,#world :hello!\r\n"

  see RFC 2812 for latest IRC client message protocol 
-}
