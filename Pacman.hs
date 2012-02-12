module Pacman where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Trans (liftIO)
import Control.Monad.State
import Data.Char
import Data.Functor.Identity
import Data.Maybe
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import qualified Data.Map as M

import Alpm.Base
import Alpm.Database

-- data Pacman = Pacman (M.Map String String)
--     deriving (Show)

comment :: (Monad m) => ParsecT String u m ()
comment = char '#' >> skipMany (noneOf "\r\n") <?> "comment"

eol :: (Monad m) => ParsecT String u m ()
eol = oneOf "\n\r" >> return () <?> "end of line"

equals :: (Monad m) => ParsecT String u m ()
equals = skipMany space >> char '=' >> skipMany space <?> "equals"

key :: (Monad m) => ParsecT String u m String
key = many1 (letter <|> digit) <?> "identifier"

pair :: (Monad m) => ParsecT String u m String
pair = rstrip <$> do
    equals
    anyChar `manyTill` (try eol <|> try comment <|> eof) <?> "value"
  where
    rstrip = reverse . dropWhile isSpace . reverse

item :: (Monad m) => ParsecT String u m (String, String)
item = do
    k <- key
    v <- fromMaybe "" <$> optionMaybe pair
    return (k, v)

header :: ParsecT String String Identity ()
header = char '[' >> many1 (letter <|> char '-') >>= putState >> char ']' >> return () <?> "header"

line :: ParsecT String String Identity (Maybe (String, (String, String)))
line = do
    skipMany space
    try (header >> return Nothing) <|> try (comment >> return Nothing) <|> item'
  where
    item' = item >>= \pair -> getState >>= \s -> return $ Just (s, pair)

file :: ParsecT String String Identity [(String, (String, String))]
file = catMaybes <$> many line

-- getPacman :: IO (Either ParseError Pacman)
getPacman = do
    input <- readFile "/etc/pacman.conf"
    return $ runParser file "options" "/etc/pacman.conf" input
    -- return $ case result of
    --    Left err -> Left err
    --    Right xs -> Right $ M.fromList (reverse xs)

-- pacmanDBs :: Pacman -> Alpm [DB]
pacmanDBs pm = return []
