{-|
Module      : Network.MPD.Client.Env
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
Stability   : unstable
Portability : unportable

Read client information from the environment.
-}

module Network.MPD.Client.Env ( getMpdRemote ) where

import           Network.MPD.Client.Remote

import           Control.Applicative ((<$>))
import           Data.Default (def)
import           System.Environment (getEnv)

-------------------------------------------------------------------------

-- | Get MPD remote information from the user's environment.
getMpdRemote :: IO MpdRemote
getMpdRemote = do
    -- WARNING: may cause your eyes to bleed ...
    portEnv <- maybeIO $ getEnv "MPD_PORT"
    hostEnv <- maybeIO $ getEnv "MPD_HOST"
    return $ (setHost hostEnv . setPort portEnv) def
    where
        setHost :: Maybe String -> MpdRemote -> MpdRemote
        setHost (Just p@('/':_)) _ = MpdPipe p
        setHost (Just "") d = d
        setHost (Just h) d =
            case sep (== '@') h of
                (name, "")  -> d { mpdHost = name }
                (pw,  name) -> d { mpdHost = name
                                 , mpdPassword = Just pw
                                 }
        setHost _ d        = d

        setPort :: Maybe String -> MpdRemote -> MpdRemote
        -- XXX: unsafe; assumes MPD_PORT is a number.
        setPort (Just p) d@(MpdHost {}) = d { mpdPort = read p }
        setPort _ d                     = d

-------------------------------------------------------------------------

maybeIO :: IO a -> IO (Maybe a)
maybeIO m = (Just <$> m) `catch` (\_ -> return Nothing)
{-# INLINE maybeIO #-}

-------------------------------------------------------------------------

-- | Like 'break' but removes the separator.
sep :: (a -> Bool) -> [a] -> ([a], [a])
sep p xs = let (a, b) = break p xs in (a, drop 1 b)
{-# INLINE sep #-}
