{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mosaic.Parser
    ( Parser
    , ParserError
    , parseUpTo
    , parseSpaces
    , parseInt
    , parseQuotes
    , runParser
    ) where

import Control.Monad.State
    ( MonadState
    , StateT
    , get
    , put
    , runStateT
    )
import Control.Monad.Except
    ( ExceptT
    , MonadError
    , runExceptT
    , throwError
    )
import Data.Char (isDigit)
import Text.Read (readMaybe)

data ParserError
    = CharError Char
    | ParseIntError String
    deriving (Show)

newtype Parser m a = Parser
    { fromParser :: StateT String (ExceptT ParserError m) a
    } deriving (Functor, Applicative, Monad, MonadError ParserError, MonadState String)

runParser :: (Monad m) => Parser m a -> String -> m (Either ParserError (a, String))
runParser p = runExceptT . runStateT (fromParser p)

parseChar :: (Monad m) => Char -> Parser m ()
parseChar c = do
    s <- get
    case s of
        (o:rest) | o == c -> put rest
        _                 -> throwError $ CharError c

parseUpTo :: (Monad m) => (Char -> Bool) -> Parser m String
parseUpTo = go ""
  where
    go :: (Monad m) => String -> (Char -> Bool) -> Parser m String
    go build p = do
        s <- get
        case s of
            (o:rest) | p o -> do
                put rest
                return $ reverse build
            (o:rest) -> do
                put rest
                go (o:build) p
            [] -> return $ reverse build

parseSpaces :: (Monad m) => Parser m ()
parseSpaces = do
    s <- get
    case s of
        (o:rest) | o == ' ' || o == '\t' || o == '\n' -> do
            put rest
            parseSpaces
        _ -> return ()

parseInt :: (Monad m) => Parser m Int
parseInt = do
    s <- parseUpTo $ not . isDigit
    case readMaybe s of
        Just i -> return i
        _      -> throwError $ ParseIntError s

parseQuotes :: (Monad m) => Char -> Parser m String
parseQuotes c = do
    parseChar c
    parseUpTo (== c)
