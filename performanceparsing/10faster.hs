{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import System.Environment
import qualified Data.Map.Strict as M
import Data.AffineSpace ((.-^))
import Data.Maybe (fromMaybe)
import Control.Lens
import Data.Word
import Data.Thyme

newtype Parser a = Parser { runParser :: forall r. BS.ByteString -> r -> (BS.ByteString -> a -> r) -> r }
                   deriving Functor

instance Applicative Parser where
    pure a = Parser $ \b _ s -> s b a
    {-# INLINE pure #-}
    Parser pf <*> Parser px = Parser $ \input failure success ->
        let succ' input' f = px input' failure (\i a -> success i (f a))
        in  pf input failure succ'
    {-# INLINE (<*>) #-}

instance Monad Parser where
    fail _ = Parser $ \_ failure _ -> failure
    m >>= k = Parser $ \input failure success ->
        let succ' input' a = runParser (k a) input' failure success
        in  runParser m input failure succ'
    {-# INLINE (>>=) #-}

getInt :: BS.ByteString -> Int
getInt = BS.foldl' (\acc n -> acc * 10 + fromIntegral (n - 0x30)) 0
{-# INLINE getInt #-}

getOctal :: BS.ByteString -> Int
getOctal = BS.foldl' (\acc n -> acc * 8 + fromIntegral (n - 0x30)) 0
{-# INLINE getOctal #-}

isDigit :: Word8 -> Bool
isDigit !x = x >= 0x30 && x <= 0x39

isUpper :: Word8 -> Bool
isUpper !x = x >= 0x41 && x <= 0x5a

decimal :: Parser Int
decimal = getInt <$> takeWhile1 isDigit
{-# INLINE decimal #-}

char :: Char -> Parser ()
char c = Parser $ \input failure success -> if BS.null input then failure else if BS8.head input == c then success (BS.tail input) () else failure
{-# INLINE char #-}

scientific :: Parser Double
scientific = finalize . BS.foldl' step (0,0) <$> takeWhile1 (\n -> isDigit n || n == 0x2e)
    where
        finalize :: (Int, Double) -> Double
        finalize (!n,!x) = if x == 0
                             then fromIntegral n
                             else fromIntegral n / x
        step (!n,!x) !v = if v == 0x2e
                              then (n,1)
                              else (n * 10 + fromIntegral (v - 0x30), x * 10)

takeWhile1 :: (Word8 -> Bool) -> Parser BS.ByteString
takeWhile1 prd = Parser $ \s failure success -> case BS.span prd s of
                                                    ("", _) -> failure
                                                    (a,b) -> success b a
{-# INLINE takeWhile1 #-}

parseOnly :: Parser a -> BS.ByteString -> Maybe a
parseOnly (Parser p) s = p s Nothing $ \b a -> if BS.null b
                                                   then Just a
                                                   else Nothing

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

findline :: ByteString -> Maybe UnixFile
findline s = case BS8.split ' ' s of
                 (inode : hardlinks : atime : mtime : ctime : user : group : blocks : ftype : perms : size : rst) -> do
                    let (path, target) = case break (== "->") rst of
                                            (a, []) -> (BS8.unwords a, Nothing)
                                            (a, ["->"]) -> (BS8.unwords a, Nothing)
                                            (a, b) -> (BS8.unwords a, Just (BS8.unwords b))
                    !atime' <- parseOnly timestamp atime
                    !mtime' <- parseOnly timestamp mtime
                    !ctime' <- parseOnly timestamp ctime
                    ft <- if BS8.length ftype == 1
                              then char2ft (BS8.head ftype)
                              else Nothing
                    return $! UnixFileGen (getInt inode)
                                          (getInt hardlinks)
                                          atime'
                                          mtime'
                                          ctime'
                                          user
                                          group
                                          (getInt blocks)
                                          ft
                                          (FPerms $! getOctal perms)
                                          (getInt size)
                                          path
                                          target
                 _ -> Nothing

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
    !tz <- takeWhile1 isUpper
    return $! case tz of
                  "CEST" -> tm .-^ fromSeconds (7200 :: Int)
                  "CET" -> tm .-^ fromSeconds (3600 :: Int)
                  _ -> tm

parseFile :: FilePath -> IO [UnixFile]
parseFile fp = fromMaybe (error "fail") . mapM findline . BS8.lines <$> BS.readFile fp

main :: IO ()
main = do
    parsed <- getArgs >>= mapM parseFile
    let resultmap = M.fromList $ map (\f -> (_filePath f, f)) $  concat parsed
    print $ M.size resultmap
