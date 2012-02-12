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
comment = char '#' *> skipMany (noneOf "\r\n") <?> "comment"

eol :: (Monad m) => ParsecT String u m ()
eol = do oneOf "\n\r"
         return ()
    <?> "end of line"

ident :: (Monad m) => ParsecT String u m String
ident = do c <- letter <|> char '_'
           cs <- many (letter <|> digit <|> char '_')
           return (c:cs)
      <?> "identifier"

item :: (Monad m) => ParsecT String u m (String, String)
item = do key <- ident
          skipMany space
          value <- optionMaybe $ rstrip <$> do
              char '='
              skipMany space
              manyTill anyChar (try eol <|> try comment <|> eof) -- TODO: fix
          return (key, fromMaybe "" value)
    where rstrip = reverse . dropWhile isSpace . reverse

takeUntil :: (Monad m) => String -> ParsecT String u m String
takeUntil s = anyChar `manyTill` lookAhead (oneOf s)

db :: ParsecT String String Identity String
db = char '[' *> takeUntil "]\r\n" <* char ']' <?> "db"

line :: ParsecT String String Identity (Maybe (String, (String, String)))
line = do
    skipMany space
    try db' <|> try (comment *> return Nothing) <|> item'

db' :: ParsecT String String Identity (Maybe (String, (String, String)))
db' = do
    sec <- db
    putState sec
    return Nothing

item' :: ParsecT String String Identity (Maybe (String, (String, String)))
item' = do
    pair <- item
    getState >>= \s -> return $ Just (s, pair)

file :: ParsecT String String Identity [(String, (String, String))]
file = catMaybes <$> many line

-- getPacman :: IO (Either ParseError Pacman)
getPacman = do
    input <- readFile "/etc/pacman.conf"
    return $ runParser file "poop" "/etc/pacman.conf" input
    -- return $ case result of
    --    Left err -> Left err
    --    Right xs -> Right $ M.fromList (reverse xs)

-- pacmanDBs :: Pacman -> Alpm [DB]
pacmanDBs pm = return []
