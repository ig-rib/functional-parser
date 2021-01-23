module HeadersParser where

import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

type Headers = [(String, String)]

--helper for parsing alphabetic chars
headerKeyChar :: Parser Char
headerKeyChar = oneOf ('-' : ['A'..'Z'] ++ ['a'..'z'])

headerValueChar :: Parser Char
headerValueChar = oneOf [' '..'~']

headerP :: Parser (String, String)
headerP = do
    key <- many1 headerKeyChar
    string ": "
    value <- many1 headerValueChar
    return (key, value)

headersP :: Parser Headers
headersP = try headersPAux <|> do
    endOfLine
    return [] 

headersPAux :: Parser Headers
headersPAux = do 
    h <- headerP
    endOfLine
    hs <- option [] headersPAux
    return (h:hs)

headersMain = do
    handle <- openFile "httpMessageHeaders.cap" ReadMode
    contents <- hGetContents handle
    print (parse headersP "" contents)