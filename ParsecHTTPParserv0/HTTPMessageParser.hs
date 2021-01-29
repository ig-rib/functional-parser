import StartLineParser
import HeadersParser
import System.IO
import System.Environment
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
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    print (parse httpMessageParser "" contents)
    print "Press any key to continue"
    getChar
