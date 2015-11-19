{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Bits
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import System.Environment
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Char (isSpace, digitToInt, isUpper)
import Data.List (foldl')

import Data.Attoparsec.Text

data UnixFile = UnixFileGen { _fileInode     :: !Int
                            , _fileHardLinks :: !Int
                            , _fileAtime     :: !UTCTime
                            , _fileMtime     :: !UTCTime
                            , _fileCtime     :: !UTCTime
                            , _fileUser      :: !Text
                            , _fileGroup     :: !Text
                            , _fileBlocks    :: !Int
                            , _fileType      :: !FileType
                            , _filePerms     :: !FPerms
                            , _fileSize      :: !Int
                            , _filePath      :: !Text
                            , _fileTarget    :: !(Maybe Text)
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

timestamp :: Parser UTCTime
timestamp = do
    y <- decimal <* char '-'
    m <- decimal <* char '-'
    d <- decimal <* char '+'
    h <- scientific <* char ':'
    mi <- scientific <* char ':'
    s <- scientific <* char '+'
    let day = fromGregorian y m d
        difftime = h * 3600 + mi * 60 + s
        tm = UTCTime day (realToFrac difftime)
    tz <- takeWhile1 isUpper <* skipSpace
    return $ case tz of
                 "CEST" -> addUTCTime (-7200) tm
                 "CET" -> addUTCTime (-3600) tm
                 _ -> tm

filetype :: Parser FileType
filetype = anyChar >>= maybe (fail "invalid file type") return . char2ft

myOctal :: Parser Int
myOctal = foldl' (\acc n -> acc * 8 + digitToInt n) 0 <$> some digit

findline :: Parser UnixFile
findline = do
    let t :: Parser a -> Parser a
        t parser = parser <* skipSpace
    meta <- UnixFileGen <$> t decimal
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
    rst <- T.words <$> t (takeWhile1 ( /= '\n' ))
    return $ case break (== "->") rst of
                 (a, []) -> meta (T.unwords a) Nothing
                 (a, ["->"]) -> meta (T.unwords a) Nothing
                 (a, b) -> meta (T.unwords a) (Just (T.unwords b))

parseFile :: FilePath -> IO [UnixFile]
parseFile fp = either (error . show) id . parseOnly (some findline) <$> T.readFile fp

main :: IO ()
main = do
    parsed <- getArgs >>= mapM parseFile
    let resultmap = M.fromList $ map (\f -> (_filePath f, f)) $  concat parsed
    print $ M.size resultmap
