{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Katip.Scribes.Rotation.Internal where

import System.IO
import System.IO.Error
import System.FilePath
import System.Directory
import Control.Monad
import Data.Maybe

import qualified Data.IntMap.Strict as IMS
import Katip

data RotationEnvironment = RotationEnvironment
    { reHandle          :: Maybe Handle
    , reDir             :: String
    , reName            :: String
    , reMaxCount        :: Int
    , reMaxSize         :: Int
    , reMap             :: IMS.IntMap FilePath
    }

scribeError :: IOError -> IO ()
scribeError = hPrint stderr

rotate :: RotationEnvironment -> IO RotationEnvironment
rotate env@RotationEnvironment{..} = catchIOError (do
        maybe (pure ()) hClose reHandle
        newMap <- IMS.fromList . ((0, reDir </> reName) :) . catMaybes <$> (mapM (moveFile reMaxCount) =<< (filterM checkExistence . IMS.toDescList $ reMap))
        handle <- openFile (reDir </> reName) WriteMode
        handle `hSetBuffering` LineBuffering
        return env{reMap=newMap, reHandle=Just handle})
        (\e -> scribeError e >> return defaultEnvironment)
    where
        checkExistence (_, name) = doesFileExist name
        moveFile maxCount (key, name)
            | key >= maxCount   = removePathForcibly name >> return Nothing
            | key == 0          = let newName = name <.> "1" in removePathForcibly newName >> renameFile name newName >> return (Just (1, newName))
            | otherwise         = let newName = name -<.> show (key + 1) in removePathForcibly newName >> renameFile name newName >> return (Just (key + 1, newName))
        defaultEnvironment = RotationEnvironment{reHandle=Nothing, reDir, reName, reMaxCount, reMaxSize, reMap=IMS.empty}
