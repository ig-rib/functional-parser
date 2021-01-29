module FirstLineParser where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Control.Applicative
import Control.Monad
import Data.Char

newtype HTTPVersion = HTTPVersion Double deriving (Show)

data HTTPFirstLine = HTTPStartLine Method RequestTarget HTTPVersion | HTTPStatusLine HTTPVersion Status deriving (Show)
newtype Method = Method String deriving (Show)
newtype RequestTarget = RequestTarget String deriving (Show)

data Status = Status Int String deriving (Show)

method :: Parser Method
method = do
    m <- choice [string "GET",
            string "PUT",
            string "POST",
            string "DELETE",
            string "HEAD", 
            string "TRACE",
            string "PATCH"]
    return (Method m)

whitespace :: Parser ()
whitespace = void $ Text.Parsec.many $ oneOf " \n\t"

lexeme2 :: Parser a -> Parser a
lexeme2 p = p <* whitespace

subdomains :: Parser String
subdomains =
        try ((:) <$> alphaNum <*> subdomains)
        Control.Applicative.<|>
        try ((++) <$> ((:) <$> alphaNum <*> string "/") <*> option "" subdomains)
        Control.Applicative.<|>
        try ((: []) <$> alphaNum) 

requestTarget :: Parser RequestTarget
requestTarget = do
    s <- char '/'
    sds <- Text.Parsec.option "" subdomains
    return (RequestTarget (s:sds))

versionParser :: Parser HTTPVersion
versionParser = do
    vno <- choice [try (string "1.0"), try (string "1.1"), string "2"]
    return (HTTPVersion (read vno))

startLine :: Parser HTTPFirstLine
startLine = do
    m <- method
    char ' '
    rt <- requestTarget
    char ' '
    string "HTTP/"
    vno <- versionParser
    return (HTTPStartLine m rt vno)
    
statusLine :: Parser HTTPFirstLine
statusLine = do
    string "HTTP/"
    version <- versionParser
    char ' '
    digits <- count 3 digit
    let st = foldl (\accum d -> 10 * accum + digitToInt d) 0 digits
    char ' '
    statusText <- many1 (oneOf (' ' : ['a'..'z'] ++ ['A'..'Z']))
    return (HTTPStatusLine version (Status st statusText))
