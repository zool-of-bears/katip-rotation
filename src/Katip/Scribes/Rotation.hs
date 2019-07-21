{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Katip.Scribes.Rotation where

import System.IO
import System.IO.Error
import System.FilePath
import System.Directory
import Text.Read

import Katip
import Katip.Scribes.Rotation.Internal

import Control.Concurrent
import Control.Monad
import Data.Maybe
import qualified Data.IntMap.Strict as IMS
import qualified Data.List as DL

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as T

data RotationSettings = RotationSettings
    { rsDir         :: String
    , rsName        :: String
    , rsMaxCount    :: Int
    , rsMaxSize     :: Int
    }

mkRotationScribe :: RotationSettings -> (forall a . LogItem a => ItemFormatter a) -> PermitFunc -> Verbosity -> IO Scribe
mkRotationScribe rotationSettings formatter permitF verbosity = do
        envMVar <- newMVar =<< initialize rotationSettings
        let rotateIfNeeded env@RotationEnvironment{..} = do
                        size <- fromInteger <$> hFileSize (fromJust reHandle)
                        if size >= reMaxSize
                            then rotate env
                            else pure env
        let logger i@Item{..} = do
                        env <- takeMVar envMVar
                        let defaultEnvironment = env{reHandle=Nothing, reMap=IMS.empty}
                        env <- if (isNothing . reHandle) env then reinitialize env else pure env
                        env <- case reHandle env of
                            Just handle -> catchIOError (do
                                    T.hPutStrLn handle $ T.toLazyText $ formatter False verbosity i
                                    rotateIfNeeded env)
                                    (\e-> scribeError e >> pure defaultEnvironment)
                            _ -> pure defaultEnvironment
                        putMVar envMVar env
        let finalizer = do
                        env <- takeMVar envMVar
                        let defaultEnvironment = env{reHandle=Nothing, reMap=IMS.empty}
                        maybe (pure ()) hClose (reHandle env)
                        putMVar envMVar defaultEnvironment
        return $ Scribe logger finalizer permitF
    where
        initialize RotationSettings{..} = catchIOError (do
                handle <- openFile (rsDir </> rsName) AppendMode
                handle `hSetBuffering` LineBuffering
                reMap <- IMS.fromList . filter predicate . fmap toPair . filter (rsName `DL.isPrefixOf`) <$> listDirectory rsDir
                return RotationEnvironment{reHandle=Just handle, reDir=rsDir, reName=rsName, reMaxCount=rsMaxCount, reMaxSize=rsMaxSize, reMap})
                (\e -> scribeError e >> return defaultEnvironment)
            where
                predicate (key, _)
                    | key <= rsMaxCount && key >= 0 = True
                    | otherwise = False
                toPair fname
                    | fname == rsName = (0, rsDir </> rsName)
                    | hasExtension fname = let maybeNumber :: Maybe Int = (readMaybe . drop 1 . takeExtension) fname in
                                            case maybeNumber of
                                                Just number -> (number, rsDir </> fname)
                                                _ -> (maxBound :: Int, rsDir </> fname)
                    | otherwise = (rsMaxCount, rsDir </> fname)
                defaultEnvironment = RotationEnvironment{reHandle=Nothing, reDir=rsDir, reName=rsName, reMaxCount=rsMaxCount, reMaxSize=rsMaxSize, reMap=IMS.empty}
        reinitialize RotationEnvironment{..} = maybe (pure ()) hClose reHandle >> initialize RotationSettings{rsDir=reDir, rsName=reName, rsMaxCount=reMaxCount, rsMaxSize=reMaxSize}
