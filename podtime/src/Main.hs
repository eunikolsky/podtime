{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import Database.SQLite.Simple
import System.Directory (getHomeDirectory)

main :: IO ()
main = do
  podcasts <- getPodcasts
  putStrLn . intercalate ", " . fmap show $ podcasts

-- | Returns a list of all podcasts in gPodder. Assumes the database
-- at the default location `~/gPodder/Database`.
getPodcasts :: IO [Int]
getPodcasts = do
  homeDir <- getHomeDirectory
  withConnection (homeDir ++ "/gPodder/Database") $ \conn -> do
    ids <- query_ conn "SELECT id FROM podcast" :: IO [Only Int]
    return $ fromOnly <$> ids
