{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-|
Module      : Happstack.Server.Session.Memory
Description : Memory storage backend for Happstack-session
Copyright   : (c) Birk Tjelmeland, 2016
License     : GPL-3
Maintainer  : birktjelmeland@yahoo.no
Stability   : experimental
Portability : POSIX

This module should ONLY be used for testing as it does not save sessions on server restarts and has other security issues.
See "Happstack.Server.Session"
-}
module Happstack.Server.Session.Memory (memoryStartSession) where

import Happstack.Server.Session

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Data.Maybe
import Data.Word
import Data.IORef


-- | Constructs value to be used with 'startSession'
memoryStartSession :: IO ((Int -> IO (Maybe (Session Int a))), (a -> Word64 -> IO (Session Int a)), (Int -> a -> IO (Maybe (Session Int a))), (Int -> IO ()))
memoryStartSession = do
    ref <- newIORef (IM.empty, 0)
    return (memorySessionGet ref, memorySessionSet ref, memorySessionUpdate ref, memorySessionDelete ref)


memorySessionGet :: (IORef (IntMap (Word64, a), Int)) -> Int -> IO (Maybe (Session Int a))
memorySessionGet ref i = do
    val <- fmap ((IM.lookup i) . fst) $ readIORef ref
    if isJust val then do
        let val' = fromJust val
        return $ Just $ Session i (fst val') (snd val')
    else
        return Nothing

memorySessionSet :: (IORef (IntMap (Word64, a), Int)) -> a -> Word64 -> IO (Session Int a)
memorySessionSet ref val len = do
    modifyIORef' ref (\(imap, i) -> (IM.insert (i+1) (len, val) imap, i+1))
    i <- fmap snd $ readIORef ref
    return $ Session i len val

memorySessionUpdate :: (IORef (IntMap (Word64, a), Int)) -> Int -> a -> IO (Maybe (Session Int a))
memorySessionUpdate ref i val = do
    (imap, index) <- readIORef ref
    let val' = IM.lookup i imap
    if isJust val' then do
        let val'' = fromJust val'
        writeIORef ref $ (IM.insert i (fst val'', val) imap, index)
        return $ Just $ Session i (fst val'') val
    else
        return Nothing

memorySessionDelete :: (IORef (IntMap (Word64, a), Int)) -> Int -> IO ()
memorySessionDelete ref i = do
    modifyIORef' ref (\(imap, index) -> (IM.delete i imap, index))
    return ()
