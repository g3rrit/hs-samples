module Lexer where

import Prelude hiding (lex)
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.Combinator

type KeyList = [String]

data State = State
  { keyList :: KeyList
  , blockStack :: [String]
  }

emptyState = State [] []

type Lexer = Parsec String State

data Token = Op String
           | Id String
           | Key String
           | IntLit Integer
           | CharLit Char
           | StringLit String
           | BStart
           | BEnd
           deriving Show

opList = [ "+", "-", "*"]

bracketList = [ ("begin", "end"), ("{", "}"), ("[", "]"), ("(", ")") ]

lex :: String -> Either ParseError [Token]
lex s = runParser top emptyState "main" s

top :: Lexer [Token]
top = white *> do 
  r <- many parseNext
  eof
  (State _ bs) <- getState
  case bs of 
    [] -> return ()
    _ -> unexpected "missing end of a block"
  return $ concat $ map (\a -> case a of { Nothing -> [] ;Just a' -> [a'] }) r

parseNext :: Lexer (Maybe Token)
parseNext = (parseMeta *> return Nothing) <|> (Just <$> parseToken)

parseToken :: Lexer Token
parseToken = (parseBStart
          <|> parseBEnd
          <|> parseOp
          <|> parseKey
          <|> parseIntLit
          <|> parseId 
          <|> parseCharLit
          <|> parseStringLit) <* white

parseBStart :: Lexer Token
parseBStart = do
  choice $ map (\(l, r) -> string l >> pushBS r) bracketList 
  return BStart 
  where pushBS v = modifyState $ \(State kl bs) -> State kl (v:bs)

parseBEnd :: Lexer Token
parseBEnd = do
  (State _ bs) <- getState
  case bs of
    []    -> unexpected "unable to find end of block"
    (b:_) -> string b >> popBS  >> return BEnd
  where popBS = modifyState $ \(State kl (b:bs)) -> State kl bs

parseOp :: Lexer Token
parseOp = Op <$> (choice $ map string opList)

parseKey :: Lexer Token
parseKey = do
  (State kl _) <- getState
  Key <$> choice (map string kl)

parseId :: Lexer Token
parseId = Id <$> many1 alphaNum

parseIntLit :: Lexer Token
parseIntLit = IntLit <$> (read <$> many1 digit)

parseCharLit :: Lexer Token
parseCharLit = CharLit <$> do
  try (char '\'')
  r <- anyChar
  char '\''
  return r

parseStringLit :: Lexer Token
parseStringLit = StringLit <$> do
  try (char '\"')
  r <- many $ noneOf "\""
  char '\"'
  return r

parseMeta :: Lexer ()
parseMeta = do
  try $ char '#'
  return ()

 
white = spaces
