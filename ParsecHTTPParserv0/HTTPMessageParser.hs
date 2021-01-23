import StartLineParser
import HeadersParser
import System.IO
import Text.Parsec
import Text.Parsec.String

data HTTPMessage = HTTPMessage HTTPStartLine Headers String deriving (Eq, Show)

httpMessageParser :: Parser HTTPMessage
httpMessageParser = do
    sL <- startLine
    endOfLine
    hs <- headersP
    endOfLine
    body <- many anyChar
    return (HTTPMessage sL hs body)

main = do
    handle <- openFile "httpMessage.cap" ReadMode
    contents <- hGetContents handle
    print (parse httpMessageParser "" contents)
