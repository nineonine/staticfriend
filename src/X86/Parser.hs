module X86.Parser where

{-
    Parse AT&T-like assembly language
-}

import Protolude
import Protolude.Partial (read)
import Data.Char
import Data.Void

import Data.Text hiding (map)
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators (between)
-- import Text.Megaparsec.Debug

import X86.AST

type Parser = Parsec Void Text

parseX86Source :: Text -> X86Program
parseX86Source str = undefined

parseOpCode :: Parser Opcode
parseOpCode = do
    space
    choice
        [ Instr <$> parseInstruction
        , (uncurry Directive) <$> parseDirective
        , Comment <$> parseComment
        , Label <$> parseLabel
        , NotImplementedOpcode <$> parseNotImpl]

parseInstruction :: Parser Instruction
parseInstruction = do
    mnemonic <- parseMnemonic
    oSize    <- parseOSize
    case numOfOperands mnemonic of
        0 -> pure (mkINstr_os0 mnemonic oSize)
        1 -> do o1 <- parseOperand
                pure (mkINstr_os1 mnemonic oSize o1)
        2 -> do o1 <- parseOperand
                o2 <- parseOperand
                pure (mkINstr_os2 mnemonic oSize o1 o2)
        3 -> panic "parseInstruction: 3 operands"
        _ -> panic "parseInstruction"

parseMnemonic :: Parser Text
parseMnemonic = choice
    ["add", "sub", "call", "lea", "mov", "push", "pop", "ret"]

parseOSize :: Parser OSize
parseOSize = readOSize <$> choice (map char ['t','q','l','w','s','b'])

parseDirective :: Parser (Text,[Text])
parseDirective = do
    void $ char '.'
    dirName :: Text <- takeWhile1P Nothing isAlphaNum
    space
    dir_args :: [Text] <-
        sepBy
            (space >> takeWhileP Nothing
                                 ((||) <$> isAlphaNum
                                       <*> (=='_'))
                   <* space)
            (char ',')
    pure (dirName, if dir_args == [""] then [] else dir_args)

parseLabel :: Parser Text
parseLabel = space >> takeWhile1P Nothing ((/=':'))

parseComment :: Parser Text
parseComment = space >> char '#' >> space >> takeWhileP Nothing (not . (==) '\n')

parseNotImpl :: Parser Text
parseNotImpl = takeRest

parseOperand :: Parser Operand
parseOperand = undefined

rEGS :: [Text]
rEGS =
    [ "rax", "rbx", "rcx", "rdx", "eax", "ebx", "ecx", "edx"
    , "ax", "bx", "cx", "dx", "ah", "bh", "ch", "dh", "al", "bl", "cl", "dl"
    , "rbp", "rsp", "rsi", "rdi", "ebp", "esp", "esi", "edi", "bp", "sp", "si", "di"
    , "cs", "ds", "es", "fs", "gs", "ss"
    ]

parseRegister :: Parser Register
parseRegister = do
    void (char '$')
    reg <- Data.Text.toUpper <$> choice (map chunk rEGS)
    return (read $ unpack reg)

-- parseMemoryOperand :: Parser MemoryOperand
-- parseMemoryOperand = do
--     s <- segment
--     void $ char ':'
--     o <- offset
--     (b, i, s) <- parens $ do
--         br <- parseRegister
--         (space >> char ',' >> space)
--         ir <- parseRegister
--         (space >> char ',' >> space)
--         s <- read <$> digitChar
--         return (br, ir, space)
--     return MemOp { segment = s
--                  , offset  = o
--                  , base    = b
--                  , index   = i
--                  , scale   = s}


parseLiteral :: Parser Literal
parseLiteral = undefined

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')
