{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import System.Environment
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.AffineSpace ((.-^))
import Data.Char (isUpper, isSpace, isDigit)
import Data.Maybe (fromMaybe)
import Control.Lens
import Data.Thyme

newtype Parser a = Parser { getParser :: BS.ByteString -> Maybe (a, BS.ByteString) }
                   deriving (Functor)

instance Applicative Parser where
    pure x = Parser (Just . (x,))
    Parser f <*> Parser x = Parser $! \s -> f s >>= \(f', s') -> x s' >>= \(x', s'') -> return $! (f' x', s'')

instance Alternative Parser where
    empty = Parser (const Nothing)
    Parser a <|> Parser b = Parser $! \s -> a s <|> b s

instance Monad Parser where
    Parser a >>= f = Parser $ \s -> a s >>= \(!a', !s') -> getParser (f a') s'

getInt :: BS.ByteString -> Int
getInt = BS.foldl' (\acc n -> acc * 10 + fromIntegral (n - 0x30)) 0

decimal :: Parser Int
decimal = getInt <$> takeWhile1 isDigit

char :: Char -> Parser ()
char c = Parser $ \s -> if BS.null s then Nothing else if BS8.head s == c then Just ((), BS.tail s) else Nothing

scientific :: Parser Double
scientific = do
    d <- decimal
    f <- fmap (fromMaybe 0) $ optional $ do
        char '.'
        s <- takeWhile1 isDigit
        let ln = BS.length s
            v = fromIntegral $ getInt s
        return (v / (10 ^ ln))
    return $! (fromIntegral d + f)

takeWhile1 :: (Char -> Bool) -> Parser BS.ByteString
takeWhile1 prd = Parser $ \s -> case BS8.span prd s of
                                    ("", _) -> Nothing
                                    (a,b) -> Just (a,b)

skipSpace :: Parser ()
skipSpace = Parser $ \s -> Just ((), BS8.dropWhile isSpace s)

anyChar :: Parser Char
anyChar = Parser $ \s -> if BS.null s then Nothing else Just (BS8.head s, BS8.tail s)

parseOnly :: Parser a -> BS.ByteString -> Maybe a
parseOnly (Parser p) s = case p s of
                             Just (output, "") -> Just output
                             _ -> Nothing

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
myOctal = BS.foldl' (\acc n -> acc * 8 + fromIntegral (n - 0x30)) 0 <$> takeWhile1 isDigit

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
parseFile fp = maybe (error "fail") id . parseOnly (some findline) <$> BS.readFile fp

main :: IO ()
main = do
    parsed <- getArgs >>= mapM parseFile
    let resultmap = M.fromList $ map (\f -> (_filePath f, f)) $  concat parsed
    print $ M.size resultmap
