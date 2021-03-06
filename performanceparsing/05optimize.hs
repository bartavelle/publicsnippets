{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import System.Environment
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.AffineSpace ((.-^))
import Data.Char (digitToInt, isUpper)
import Data.List (foldl')
import Control.Lens
import Data.Thyme

import Data.Attoparsec.ByteString.Char8

data UnixFile = UnixFileGen { _fileInode     :: !Int
                            , _fileHardLinks :: !Int
                            , _fileAtime     :: !UTCTime
                            , _fileMtime     :: !UTCTime
                            , _fileCtime     :: !UTCTime
                            , _fileUser      :: !ByteString
                            , _fileGroup     :: !ByteString
                            , _fileBlocks    :: !Int
                            , _fileType      :: !FileType
                            , _filePerms     :: !FPerms
                            , _fileSize      :: !Int
                            , _filePath      :: !ByteString
                            , _fileTarget    :: !(Maybe ByteString)
                            } deriving (Show, Eq)

data FileType = TFile
              | TDirectory
              | TLink
              | TPipe
              | TSocket
              | TBlock
              | TChar
              | TDoor
              deriving (Eq, Show)

char2ft :: Char -> Maybe FileType
char2ft x = case x of
                '-' -> Just TFile
                'f' -> Just TFile
                'd' -> Just TDirectory
                'l' -> Just TLink
                'p' -> Just TPipe
                's' -> Just TSocket
                'b' -> Just TBlock
                'c' -> Just TChar
                'D' -> Just TDoor
                _ -> Nothing

newtype FPerms = FPerms Int
               deriving (Show, Ord, Eq, Num, Bits)

parseYMD :: Parser Day
parseYMD = do
    !y <- decimal <* char '-'
    !m <- decimal <* char '-'
    !d <- decimal
    return $! YearMonthDay y m d ^. from gregorian

parseDTime :: Parser DiffTime
parseDTime = do
    !h  <- decimal <* char ':'
    !mi <- decimal <* char ':'
    !s  <- scientific
    return $! fromSeconds $ fromIntegral (h * 3600 + mi * 60 :: Int) + s

timestamp :: Parser UTCTime
timestamp = do
    !day <- parseYMD <* char '+'
    !difftime <- parseDTime <* char '+'
    let !tm = UTCTime day difftime ^. from utcTime
    !tz <- takeWhile1 isUpper <* skipSpace
    return $! case tz of
                  "CEST" -> tm .-^ fromSeconds (7200 :: Int)
                  "CET" -> tm .-^ fromSeconds (3600 :: Int)
                  _ -> tm

filetype :: Parser FileType
filetype = anyChar >>= maybe (fail "invalid file type") return . char2ft

myOctal :: Parser Int
myOctal = foldl' (\acc n -> acc * 8 + digitToInt n) 0 <$> some digit

findline :: Parser UnixFile
findline = do
    let t :: Parser a -> Parser a
        t parser = parser <* skipSpace
    !meta <- UnixFileGen <$> t decimal
                         <*> t decimal
                         <*> timestamp
                         <*> timestamp
                         <*> timestamp
                         <*> t (takeWhile1 (not . isSpace))
                         <*> t (takeWhile1 (not . isSpace))
                         <*> t decimal
                         <*> t filetype
                         <*> (FPerms <$> t myOctal)
                         <*> t decimal
    !rst <- BS8.words <$> t (takeWhile1 ( /= '\n' ))
    return $! case break (== "->") rst of
                  (a, []) -> meta (BS8.unwords a) Nothing
                  (a, ["->"]) -> meta (BS8.unwords a) Nothing
                  (a, b) -> meta (BS8.unwords a) (Just (BS8.unwords b))

parseFile :: FilePath -> IO [UnixFile]
parseFile fp = either (error . show) id . parseOnly (some findline) <$> BS.readFile fp

main :: IO ()
main = do
    parsed <- getArgs >>= mapM parseFile
    let resultmap = M.fromList $ map (\f -> (_filePath f, f)) $  concat parsed
    print $ M.size resultmap
