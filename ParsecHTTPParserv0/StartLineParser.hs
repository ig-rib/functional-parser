import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative
import Control.Monad

newtype Method = Method String deriving (Eq, Show)
newtype RequestTarget = RequestTarget String deriving (Eq, Show)
newtype HTTPVersion = HTTPVersion Double deriving (Eq, Show)
data HTTPStartLine = HTTPStartLine Method RequestTarget HTTPVersion deriving (Eq, Show)

lexer = makeTokenParser emptyDef

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

requestTarget :: Parser RequestTarget
requestTarget = do
    s <- string "/"
    return (RequestTarget s)

startLine :: Parser HTTPStartLine
startLine = do
    m <- lexeme2 method
    rt <- lexeme2 requestTarget
    return (HTTPStartLine m rt (HTTPVersion 1.1))