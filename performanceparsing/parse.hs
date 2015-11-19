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
import Data.Char (isSpace, digitToInt)
import Data.Functor.Identity
import Data.List (foldl')

import Text.Parsec.Text
import Text.Parsec.Char
import qualified Text.Parsec.Token as TOK
import Text.Parsec hiding (many, (<|>), optional)

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
                            , _filePath      :: !FilePath
                            , _fileTarget    :: !(Maybe FilePath)
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
    y <- parseInt <* char '-'
    m <- parseInt <* char '-'
    d <- parseInt <* char '+'
    h <- parseInt <* char ':'
    mi <- parseInt <* char ':'
    s <- realToFrac <$> TOK.float tok <* char '+'
    let day = fromGregorian y m d
        difftime = h * 3600 + mi * 60 + s
        tm = UTCTime day difftime
    tz <- some upper <* spaces
    return $ case tz of
                 "CEST" -> addUTCTime (-7200) tm
                 "CET" -> addUTCTime (-3600) tm
                 _ -> tm

filetype :: Parser FileType
filetype = anyChar >>= maybe (fail "invalid file type") return . char2ft

tok :: TOK.GenTokenParser Text () Identity
tok = TOK.makeTokenParser TOK.LanguageDef
        { TOK.commentStart   = ""
        , TOK.commentEnd     = ""
        , TOK.commentLine    = ""
        , TOK.nestedComments = True
        , TOK.identStart     = letter <|> char '_'
        , TOK.identLetter    = alphaNum <|> oneOf "_'"
        , TOK.opStart        = alphaNum
        , TOK.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , TOK.reservedOpNames= []
        , TOK.reservedNames  = []
        , TOK.caseSensitive  = True
        }

parseInt :: Num a => Parser a
parseInt = fromIntegral <$> TOK.integer tok

myOctal :: Parser Int
myOctal = foldl' (\acc n -> acc * 8 + digitToInt n) 0 <$> some digit

findline :: Parser UnixFile
findline = do
    let t :: Parser a -> Parser a
        t parser = parser <* spaces
    meta <- UnixFileGen <$> parseInt
                        <*> parseInt
                        <*> timestamp
                        <*> timestamp
                        <*> timestamp
                        <*> t (T.pack <$> some (satisfy (not . isSpace)))
                        <*> t (T.pack <$> some (satisfy (not . isSpace)))
                        <*> parseInt
                        <*> t filetype
                        <*> (FPerms <$> myOctal)
                        <*> parseInt
    rst <- words <$> t (some (satisfy ( /= '\n' )))
    return $ case break (== "->") rst of
                 (a, []) -> meta (unwords a) Nothing
                 (a, ["->"]) -> meta (unwords a) Nothing
                 (a, b) -> meta (unwords a) (Just (unwords b))

parseFile :: FilePath -> IO [UnixFile]
parseFile fp = either (error . show) id . parse (many findline <* eof) fp <$> T.readFile fp

main :: IO ()
main = do
    parsed <- getArgs >>= mapM parseFile
    let resultmap = M.fromList $ map (\f -> (_filePath f, f)) $  concat parsed
    print $ M.size resultmap
