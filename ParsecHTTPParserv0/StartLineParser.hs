module StartLineParser where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Control.Applicative
import Control.Monad

newtype Method = Method String deriving (Eq, Show)
newtype RequestTarget = RequestTarget String deriving (Eq, Show)
newtype HTTPVersion = HTTPVersion Double deriving (Eq, Show)
data HTTPStartLine = HTTPStartLine Method RequestTarget HTTPVersion deriving (Eq, Show)


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

startLine :: Parser HTTPStartLine
startLine = do
    m <- method
    char ' '
    rt <- requestTarget
    char ' '
    string "HTTP/"
    vno <- choice [try (string "1.0"), try (string "1.1"), string "2"]
    return (HTTPStartLine m rt (HTTPVersion (read vno)))