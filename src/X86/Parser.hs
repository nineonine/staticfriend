module X86.Parser where

{-
    Parse AT&T-like assembly language
-}

import Protolude hiding (try)
import Protolude.Partial (read)

import Data.Text hiding (map)
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
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
    space
    mnemonic <- parseMnemonic
    oSize    <- parseOSize
    space
    case numOfOperands mnemonic of
        0 -> pure (mkINstr_os0 mnemonic oSize)
        1 -> do o1 <- parseOperand
                pure (mkINstr_os1 mnemonic oSize o1)
        2 -> do o1 <- parseOperand
                (char ',' >> space)
                -- can't have immediates in dest
                o2 <- choice [ Reg <$> parseRegister
                             , Memory <$> parseMemoryOperand]
                pure (mkINstr_os2 mnemonic oSize o1 o2)
        3 -> panic "parseInstruction: 3 operands"
        _ -> panic "parseInstruction"

parseMnemonic :: Parser Text
parseMnemonic = choice
    ["add", "sub", "call", "lea", "mov", "push", "pop", "ret", "xor"]

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
parseOperand = choice [
    Immediate <$> parseLiteral
  , Reg <$> parseRegister
  , Memory <$> parseMemoryOperand
  ]

rEGS :: [Text]
rEGS =
    [ "rax", "rbx", "rcx", "rdx", "eax", "ebx", "ecx", "edx"
    , "ax", "bx", "cx", "dx", "ah", "bh", "ch", "dh", "al", "bl", "cl", "dl"
    , "rbp", "rsp", "rsi", "rdi", "ebp", "esp", "esi", "edi", "bp", "sp", "si", "di"
    , "cs", "ds", "es", "fs", "gs", "ss", "rip", "ip"
    ]

parseRegister :: Parser Register
parseRegister = do
    void (char '%')
    reg <- Data.Text.toUpper <$> choice (map chunk rEGS)
    return (read $ unpack reg)

-- https://sourceware.org/binutils/docs/as/i386_002dMemory.html
parseMemoryOperand :: Parser MemoryOperand
parseMemoryOperand = do
    -- TODO: this should be restricted to segment registers only: ["cs", "ds", "es", "fs", "gs", "ss"]
    s <- optional parseRegister
    when (isJust s) (void (char ':'))
    d <- optional $ try $
        (I <$> L.signed space L.decimal) <|> (Lbl <$> takeWhile1P Nothing ((/='(')))
    res <- optional $ parens $ do
        basereg <- optional (try parseRegister)
        (space >> optional (char ',') >> space)
        indexreg <- optional parseRegister
        (space >> optional (char ',') >> space)
        scale <- optional L.decimal -- TODO: accept 1, 2, 4, and 8 only
        return (basereg, indexreg, scale)
    let (b, i, sc) = fromMaybe (Nothing,Nothing,Nothing) res
    return MemOp { segment = s
                 , disp    = d
                 , base    = b
                 , index   = i
                 , scale   = sc}


parseLiteral :: Parser Literal
parseLiteral = do
    void $ char '$'
    choice [
        (try $ D <$> L.signed space L.float) <|>
        (I <$> L.signed space L.decimal)
      , Lbl <$> parseLabel
      ]

--------------------------------------------------------------------

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')
