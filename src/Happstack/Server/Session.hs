{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
{-|
Module      : Happstack.Server.Session
Description : Serverside sessions for Happstack.
Copyright   : (c) Birk Tjelmeland, 2016
License     : MIT
Maintainer  : birktjelmeland@yahoo.no
Stability   : experimental
Portability : POSIX

Serverside sessions for Happstack. Curently highly experimental and api might change without notice. Must be used together with a Storage Backend. See "Happstack.Server.Session.Memory" as an example.
-}
module Happstack.Server.Session (Session(..), SessionConfig(..), mkSessionConfig, SessionHandler, startSession, getSession, setSession, updateSession, deleteSession) where

import Prelude hiding (lookup)
import Data.Word
import Data.Maybe
import Data.Text (Text)
import Control.Monad

import Data.Either

import Control.Monad.IO.Class

import Data.Time.Clock.POSIX

import Control.Exception

import Crypto.Cipher.Types
import Crypto.Cipher.AES
import Crypto.Error
import Crypto.Data.Padding

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16 (encode, decode)

import Happstack.Server hiding (Session, host)


maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


-- | Session
data Session a b = Session {
        sessionId     :: a,
        sessionExpire :: Word64,
        sessionData   :: b
    } deriving (Show)

--type GetSession    = (a -> IO (Session a b))
--type SetSession    = (b -> Word64 -> IO (Session a b))
--type UpdateSession = (a -> b -> IO (Session a b))
--type DeleteSession = (a -> IO ())

-- | Configuration for session. See 'mkSessionConfig'
data SessionConfig a = SessionConfig {
        sessionAuthEncrypt :: (a -> String),
        sessionAuthDecrypt :: (String -> Maybe a)
    }

-- | Make the 'SessionConfig' to be used with startSession.
--  Uses AES128 cipher in cbc mode to encrypt session IDs.
--  This function will fail with a error if a invalid key or IV is used.
--  The AES key and IV pair can be constructed using the OpenSSL command where secret is the password you would like to use.
--  See https://www.openssl.org/docs/manmaster/apps/enc.html
--
-- >>> openssl enc -aes-128-cbc -k secret -P -md sha256
-- salt=63BDA9D94554A072
-- key=F4FCD1AA73DE4A31135668B4F2428AC3
-- iv =98EDE03AB48FC1F8BECA84D5F98A12F2
--
mkSessionConfig :: (Read a, Show a) =>
                   BS.ByteString   -- ^ AES128 Cipher key in Base16
                -> BS.ByteString   -- ^ AES IV in Base16
                -> SessionConfig a -- ^ 'SessionConfig' to be used with 'startSession'
mkSessionConfig key iv = SessionConfig {
        sessionAuthEncrypt = encrypt,
        sessionAuthDecrypt = decrypt
    }
    where
        cipher = either (\m -> error $ show m ++ "\nInvalid key passed to mkSessionConfig") (\x -> x) $ eitherCryptoError $ cipherInit key :: AES128
        iv'    = fromMaybe (error "Invalid IV passed to mkSessionConfig") $ makeIV $ fst $ decode iv :: IV AES128
        encrypt m = C.unpack $ encode $ cbcEncrypt cipher iv' $ pad (PKCS7 16) $ C.pack $ show m
        decrypt c | (BS.length $ fst $ decode $ C.pack c) `mod` 16 == 0 = read . C.unpack <$> (unpad (PKCS7 16) $ cbcDecrypt cipher iv' $ fst $ decode $ C.pack c)
                  | otherwise                                           = Nothing

-- | Session handler to be used with 'getSession', 'setSession', 'updateSession' and 'deleteSession'
data SessionHandler a b = SessionHandler (SessionConfig a) (a -> IO (Maybe (Session a b))) (b -> Word64 -> IO (Session a b)) (a -> b -> IO  (Maybe (Session a b))) (a -> IO ())

-- | Creates a 'SessionHandler' from 'SessionConfig' and a session handler constructor
-- Example:
--
-- > import Happstack.Server.Session
-- > import Happstack.Server.Session.Memory -- ONLY USE FOR TESTING
-- >
-- > main = do
-- >     sessionHandler <- startSession (mkSessionConfig "F4FCD1AA73DE4A31135668B4F2428AC3" "98EDE03AB48FC1F8BECA84D5F98A12F2") memoryStartSession
--
startSession :: SessionConfig a -- ^ Session configuration. See 'mkSessionConfig'
            -> IO ((a -> IO (Maybe (Session a b))), (b -> Word64 -> IO (Session a b)), (a -> b -> IO (Maybe (Session a b))), (a -> IO ())) -- ^ Session handler constructor
            -> IO (SessionHandler a b) -- ^ Session handler to be used with 'getSession', 'setSession', 'updateSession' and 'deleteSession'
startSession sessionConfig sessionHandler = do
    (getSession, setSession, updateSession, deleteSession) <- sessionHandler
    return $ SessionHandler sessionConfig getSession setSession updateSession deleteSession

-- | Gets session in a request. If the session ID is invalid or no session is found 'Nothing' is returned.
getSession :: (MonadPlus m, MonadIO m, FilterMonad Response m, HasRqData m, Read a) => (SessionHandler a b) -> m (Maybe b)
getSession sessionHandler@(SessionHandler sessionConfig getSession' _ _ _) = msum
    [ do
        sid' <- lookCookieValue "SID"
        let sid = sessionAuthDecrypt sessionConfig sid'
        session <- maybe (return Nothing) (liftIO . getSession') sid
        timeNow <- liftIO $ fmap (floor) getPOSIXTime
        if ( fromMaybe False $ (> timeNow) . sessionExpire <$> session ) then do
            deleteSession sessionHandler
            return Nothing
        else
            return $ sessionData <$> session
    , do
        return Nothing
    ]

-- | Sets a session. DO NOT USE this function if user is not verified in some sort of way, by login, chapta, etc. Current versions of Happstack-session do not preform automatic deletions on outdated sessions which may pose a security risk if all users are allowed to register a session without verification.
setSession :: (MonadIO m, FilterMonad Response m, Show a) =>
              (SessionHandler a b)
           -> b      -- ^ Session data
           -> Word64 -- ^ Session lifetime in seconds
           -> m ()
setSession  (SessionHandler sessionConfig _ setSession' _ _) dat expiry = do
    timeNow <- liftIO $ fmap (floor) getPOSIXTime
    session <- liftIO $ setSession' dat (timeNow + expiry)
    addCookie (MaxAge $ fromIntegral expiry) $ mkCookie "SID" $ sessionAuthEncrypt sessionConfig $ sessionId session
    return ()

-- | Updates session value. Note: current versions of Happstack-session do not allow for updating session expiry
updateSession :: (MonadPlus m, MonadIO m, FilterMonad Response m, HasRqData m, Read a) =>
                 (SessionHandler a b)
              -> b -- ^ New session data
              -> m ()
updateSession sessionHandler@(SessionHandler sessionConfig _ _ updateSession' _) dat = msum
    [ do
        sid' <- lookCookieValue "SID"
        let sid = sessionAuthDecrypt sessionConfig sid'
        session <- maybe (return Nothing) (liftIO . (`updateSession'` dat)) sid
        timeNow <- liftIO $ fmap (floor) getPOSIXTime
        if (fromMaybe False $ (> timeNow) . sessionExpire <$> session) then do
            deleteSession sessionHandler
            return ()
        else
            return ()
    , do
        return ()
    ]

-- | Deletes session
deleteSession :: (MonadPlus m, MonadIO m, FilterMonad Response m, HasRqData m, Read a) => (SessionHandler a b) -> m ()
deleteSession (SessionHandler sessionConfig _ _ _ deleteSession') = msum
    [ do
        sid' <- lookCookieValue "SID"
        let sid = sessionAuthDecrypt sessionConfig sid'
        maybe (return ()) (liftIO . deleteSession') sid
        expireCookie "SID"
        return ()
    , do
        return ()
    ]
