module X86.Parser where

import Data.Void

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators (between)

import X86.AST

type Parser = Parsec Void Text

parseX86Source :: Text -> X86Program
parseX86Source str = undefined

parseOpCode :: Parser Opcode
parseOpCode = undefined
-- parseOpCode = parseInstruction
--             <|> parseDirective
--             <|> parseLabel
--             <|> parseNotImpl

parseInstruction :: Parser Instruction
parseInstruction = do
    -- sc
    -- dot
    -- i <- manyTill alphaNumChar spaceChar
    -- sc
    -- rest <- takeWhile (not <$> eol)
    undefined

parseOperand :: Parser Operand
parseOperand = undefined

parseOperandSize :: Parser OSize
parseOperandSize = undefined

parseMemoryOperand :: Parser MemoryOperand
parseMemoryOperand = undefined

parseLiteral :: Parser Literal
parseLiteral = undefined

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "#")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pInteger :: Parser Integer
pInteger = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

dot :: Parser Text
dot = symbol "."
