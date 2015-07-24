{-# LANGUAGE OverloadedStrings, CPP, ExistentialQuantification, DeriveDataTypeable #-}

module Web.Authenticate.SQRL.Client.Daemon where

import Network.Socket (SockAddr(SockAddrInet))
import Network.Wai (ResponseReceived(), responseLBS, responseFile, Application, remoteHost, rawPathInfo, rawQueryString, requestHeaders)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort, setNoParsePath)
import Network.HTTP.Types (mkStatus, status200, status303, status403, status404)
import Network.HTTP.Types.Header (hContentType, hLocation, hReferer, hServer)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.Process (waitForProcess, runInteractiveCommand)
import Control.Applicative
import System.Exit (ExitCode(..))
import Data.Char (isSpace, isNumber)
import Data.Word (Word8)
--import Control.Concurrent (threadDelay)
import System.IO (stderr, hClose, hSetNewlineMode, universalNewlineMode)

import qualified Data.CaseInsensitive as CI
import Data.Bits
import System.Posix.Types (Fd)
import System.Posix.User (UserEntry, getUserEntryForID, userName, userShell, homeDirectory, setUserID, setGroupID, userID, userGroupID)
import System.Posix.Files (getSymbolicLinkStatus, fileExist, fileOwner, fileExist, readSymbolicLink)
import System.Posix.Process (forkProcess, createSession, executeFile)
import System.Posix.IO (createPipe, dupTo, stdInput, stdOutput, stdError, closeFd, fdToHandle, fdWrite)
import Control.Monad (when)
import Control.Exception (handle, toException, fromException, throwIO, Exception, SomeException)
import Data.Typeable

data SomeCPSException = forall e . Exception e => SomeCPSException e
    deriving (Typeable)

instance Show SomeCPSException where
    show (SomeCPSException e) = show e

instance Exception SomeCPSException

cpsExceptionToException :: Exception e => e -> SomeException
cpsExceptionToException = toException . SomeCPSException

cpsExceptionFromException :: Exception e => SomeException -> Maybe e
cpsExceptionFromException x = do
    SomeCPSException a <- fromException x
    cast a

data CPSMandatoryHeaderMismatch = CPSMandatoryHeaderMismatch deriving (Typeable, Show)
instance Exception CPSMandatoryHeaderMismatch where
  toException   = cpsExceptionToException
  fromException = cpsExceptionFromException

data CPSUrlLengthMismatch = CPSUrlLengthMismatch { cpsExpectedLength :: Int, cpsActualLength :: Int } deriving (Typeable, Show)
instance Exception CPSUrlLengthMismatch where
  toException   = cpsExceptionToException
  fromException = cpsExceptionFromException


main :: IO ()
main = do
  let port = 25519
      host = "127.0.0.1"
      sett = setNoParsePath True $ setHost host $ setPort port defaultSettings
  putStrLn $ "Listening on " ++ show host ++ ":" ++ show port
  runSettings sett app

app :: Application
app req f =
  case remoteHost req of
   SockAddrInet port shouldBeLocal ->
     if shouldBeLocal /= localhost
     then conFail "Not localhost"
     else if rawPathInfo req == "/favicon.ico" then sendIcon else lookupPortUser port >>= \x -> case x of
       Nothing -> conFail "No port lookup"
       Just (prog, fullpath, user, env) ->
         let withReferer = case lookup hReferer $ requestHeaders req of
               Just r -> (:) ("--cps-referer=" ++ T.unpack (TE.decodeUtf8 r))
               Nothing -> id
             fullLink = BS.dropWhile isSlash $ rawPathInfo req `BS.append` rawQueryString req
             isSlash = (==) (fromIntegral (fromEnum '/') :: Word8)
         in handleFor port user env $ withReferer
            [ "--cps-daemon-flags=3"
            , "--cps-user=" ++ userName user
            , "--cps-invoker=" ++ T.unpack prog
            , "--cps-invoker-full=" ++ T.unpack fullpath
            , "--cps-port=" ++ show port
            , "--cps-query-string=" ++ T.unpack (TE.decodeUtf8 $ rawQueryString req)
            , "--cps-path-info=" ++ T.unpack (TE.decodeUtf8 $ rawPathInfo req)
            , "sqrl://" ++ T.unpack (TE.decodeUtf8 fullLink)
            ]
   _ -> conFail "Not IPv4"
  where conFail = f . responseLBS status403 [(hContentType, "text/plain"), serverHdr]
        handleFor :: (Show port, Integral port) => port -> UserEntry -> [(T.Text, T.Text)] -> [String] -> IO ResponseReceived
        handleFor port user env args = do
#ifndef SQRL_CPS_NOUSERCLIENT
          let uexe = homeDirectory user ++ "/.sqrl/bin/sqrl-cps"
          fileExist uexe >>= \uflxt -> if uflxt then executeAs port user env args uexe else do
#endif
            let exe = "/bin/sqrl-cps"
            fileExist exe >>= \flxt ->
              if flxt then executeAs port user env args exe
              else nothingToExecute port user env args exe
        nothingToExecute :: (Show port, Integral port) => port -> UserEntry -> [(T.Text, T.Text)] -> [String] -> String -> IO ResponseReceived
        nothingToExecute _ user env _ exe =
          f $ responseLBS status200 [(hContentType, "text/plain;charset=utf-8")] $ LBS.fromChunks
            ("The user " : TE.encodeUtf8 (T.pack $ userName user) : " could not execute the file " : TE.encodeUtf8 (T.pack exe) : " because it does not exist.\n"
            : map (\(a, b) -> BS.concat ["\n", TE.encodeUtf8 a, "=", TE.encodeUtf8 b]) env)
        executeAs :: (Show port, Integral port) => port -> UserEntry -> [(T.Text, T.Text)] -> [String] -> String -> IO ResponseReceived
        executeAs port user env args exe = do
          (thisread, procwrite) <- createPipe -- create pipes for interprocess communication
          (procread, thiswrite) <- createPipe
          (thiserr,  procerr)   <- createPipe
          _ <- forkProcess $ executeAs' port user env args exe procread procwrite procerr
          inputh <- fdToHandle thisread
          closeFd procwrite
          closeFd procread
          closeFd procerr
          hSetNewlineMode inputh universalNewlineMode
          mandatory <- BS.hGet inputh 8
          when (BS.length mandatory /= 8 || BS.take 7 mandatory /= "SQRLCPS") $ throwIO CPSMandatoryHeaderMismatch
          let tc = BS.last mandatory -- get client flags
          
          -- flag 0x01 indicates that the process will send HTTP headers (starting with the status "HTTP/1.1 xxx description"),
          -- otherwise it will send a 16 bit unsigned int (with no more than 15 bits in use) in network byte order (big-endian)
          --  followed by that many bytes representing the URI to do a 303 redirect to.
          (stts, hdrs) <- if (tc .&. 1) /= 0 then readHeaders inputh
            else (\x -> (status303, [(hContentType, "text/html;charset=utf-8"), (hLocation, x)])) <$> do
                 urilen <- BS.unpack <$> BS.hGet inputh 2 >>= \bytes -> return $ case bytes of
                   [high, low] -> (fromIntegral high `shiftL` 8) .|. (fromIntegral low :: Int)
                   _ -> -1
                 uri <- BS.hGet inputh urilen
                 when (BS.length uri /= urilen) $ throwIO $ CPSUrlLengthMismatch urilen $ BS.length uri
                 return uri
          
          -- close the writing end of the new process' stdin
          closeFd thiswrite

          -- flag 0x02 indicates that the process will use the rest of the communication to write the body
          body <- if (tc .&. 2) /= 0 then LBS.hGetContents inputh else return $ defaultBody hdrs
          --hClose inputh
          closeFd thisread
          errh <- fdToHandle thiserr
          errlines <- LBS.intercalate (LBS.append (LBS.fromStrict $ TE.encodeUtf8 $ T.pack exe) ": ") . LBS.groupBy (\a _ -> 10 /= a) <$> LBS.hGetContents errh
          LBS.hPutStr stderr errlines
          hClose errh
          closeFd thiserr
          
          -- now everything is done
          f $ responseLBS stts (serverHdr : hdrs) body
        defaultBody hdrs = case lookup hLocation hdrs of
          Just loc -> LBS.fromChunks ["Continue by <a href=\"", loc, "\">clicking here</a>."]
          Nothing  -> "No location given by client."
        bsint = BS.foldl' (\a b -> a * 10 + fromIntegral b - 48) 0
        readHeaders h = do
          statusLine <- BS.hGetLine h >>= \fullstatus -> return $ (\(a, b) -> mkStatus (bsint a) (BS.tail b)) $ BS.breakByte 32 $
            if BS.take 6 fullstatus == "HTTP/1" then BS.tail $ snd $ BS.breakByte 32 fullstatus else fullstatus
          ls <- map (\x -> let (a, b) = BS.breakByte (fromIntegral $ fromEnum ':') x in (CI.mk $ fst $ BS.spanEnd (==32) a, BS.dropWhile (==32) $ BS.tail b)) <$> listUntilM BS.null (BS.hGetLine h) []
          return (statusLine, ls)
        listUntilM :: (a -> Bool) -> IO a -> [a] -> IO [a]
        listUntilM tf mf as = mf >>= \a -> if tf a then return as else listUntilM tf mf (a : as)

        executeAs' :: (Show port, Integral port) => port -> UserEntry -> [(T.Text, T.Text)] -> [String] -> String -> Fd -> Fd -> Fd -> IO ()
        executeAs' _ user env args exe fdInput fdOutput fdError = do
          let logf = homeDirectory user ++ "/.sqrl/daemon.log"
          BS.writeFile logf "Started process...\n"
          dupTo fdInput  stdInput
          dupTo fdOutput stdOutput
          dupTo fdError  stdError
          closeFd fdInput
          closeFd fdOutput
          closeFd fdError
          LBS.appendFile logf $ LBS.fromChunks ["Replacing executable ", TE.encodeUtf8 $ T.pack exe, ".\n"]
          handle (\e -> BS.appendFile logf (BS.append "Exception: " $ TE.encodeUtf8 $ T.pack $ show (e :: SomeException)) >> fdWrite stdOutput "\0\0\0\0\0\0\0\0\0\0" >> closeFd stdOutput >> closeFd stdInput >> closeFd stdError >> throwIO e) $ do
            setUserID $ userID user
            setGroupID $ userGroupID user
            createSession
            executeFile exe False args (Just $ map (\(a, b) -> (T.unpack a, T.unpack b)) env)
          BS.appendFile logf "Replacing executable failed.\n"
        lookupPortUser port = do
          (hin, hout, herr, pid) <- runInteractiveCommand $ "fuser -n tcp " ++ show port
          putStrLn $ "Connection from :" ++ show port
          hClose hin
          --threadDelay 20000000
          code <- waitForProcess pid
          hClose herr
          ls' <- (T.unpack . T.takeWhile isNumber . T.dropWhile isSpace) <$> TIO.hGetContents hout
          if code /= ExitSuccess || null ls' then hClose hout >> return Nothing
            else do
                 hClose hout
                 let envfile = "/proc/" ++ ls' ++ "/environ"
                     exefile = "/proc/" ++ ls' ++ "/exe"
                     comfile = "/proc/" ++ ls' ++ "/comm"
                 ownerid <- fileOwner <$> getSymbolicLinkStatus envfile
                 owner <- getUserEntryForID ownerid
                 print owner
                 fullpath <- T.pack <$> readSymbolicLink exefile
                 comm <- TIO.readFile comfile
                 env <- (filter copyEnv . map (\x -> let (a, b) = T.break (=='=') x in (a, if T.null b then T.empty else T.tail b)) . filter (not . T.null) . T.splitOn "\0") <$> TIO.readFile envfile
                 let username = T.pack $ userName owner
                     homedir  = T.pack $ homeDirectory owner
                 return $ Just (comm, fullpath, owner, ("LOGNAME", username) : ("USER", username) : ("PWD", homedir) : ("HOME", homedir) : ("SHELL", T.pack $ userShell owner) : env)
        copyEnv :: (T.Text, T.Text) -> Bool
        copyEnv (a, _) = T.isPrefixOf "LC_" a || T.isPrefixOf "XDG_" a || T.isPrefixOf "SSH_" a || elem a
          [ "DISPLAY", "GTK_IM_MODULE", "QT_IM_MODULE", "QT4_IM_MODULE", "QT_QPA_PLATFORMTHEME", "CLUTTER_IM_MODULE", "XMODIFIERS", "SELINUX_INIT", "GPG_AGENT_INFO"
          , "UPSTART_SESSION", "GNOME_KEYRING_CONTROL", "GTK_MODULES", "SESSION_MANAGER", "DEFAULTS_PATH", "DESKTOP_SESSION", "JOB", "LANG", "GNOME_KEYRING_PID"
          , "GDM_LANG", "MANDATORY_PATH", "GDMSESSION", "IM_CONFIG_PHASE", "COMPIZ_CONFIG_PROFILE", "SESSIONTYPE", "LANGUAGE", "GNOME_DESKTOP_SESSION_ID"
          , "COMPIZ_BIN_PATH", "DBUS_SESSION_BUS_ADDRESS", "TEXTDOMAIN", "TEXTDOMAINDIR", "XAUTHORITY"
          ]
        localhost = 0x0100007F
        serverHdr = (,) hServer $ BS.intercalate " "
          [ "sqrl-auth-daemon/0.1"
#ifdef VERSION_wai
          , BS.append "WAI/" VERSION_wai
#endif
#ifdef VERSION_warp
          , BS.append "warp/" VERSION_warp
#endif
          ]
        sendIcon = do
          (stts, file) <-
            fileExist "/usr/share/sqrl/sqrl.ico" >>= \t -> return $
              if t then (status200, "/usr/share/sqrl/sqrl.ico") else (status404, "/dev/null")
          f $ responseFile stts [(hContentType, "image/x-icon"), serverHdr] file Nothing
