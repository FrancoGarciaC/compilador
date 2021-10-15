{-|
Module      : Parse
Description : Define un parser de términos FD40 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (stm, Parse.parse, sdecl, runP, P, program, declOrSintypeOrStm,typeP,sinTypeProgram) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec
    ( ParseError,
      eof,
      option,
      sourceColumn,
      sourceLine,
      (<|>),
      getPosition,
      many,
      runParser,
      try,
      Parsec,
      ParsecT )
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Expr (Operator, Assoc)
import Control.Monad.Identity (Identity)
import Elab (buildType)
import Data.Either

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "fun", "fix", "then", "else","in",
                           "ifz", "print","Nat"],
         reservedOpNames = ["->",":","=","+","-"]
        }


-- creamos un nuevo lexer que no tenga como palabras reservadas Nat y ->
lexerST ::Tok.TokenParser u
lexerST = Tok.makeTokenParser $
          emptyDef {
          commentLine    = "#",
          reservedNames = ["let", "fun", "fix", "then", "else","in",
                           "ifz", "print"],
          reservedOpNames = [":","=","+","-"]
          }


whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

identifierST :: P String
identifierST = Tok.identifier lexerST


reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier

-- Esto se usa para sinonimos de tipo
varST :: P Name
varST = identifierST


getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P Ty
tyatom = (reserved "Nat" >> return NatTy)
         <|> parens typeP

typeP :: P Ty
typeP = try (do x <- tyatom
                reservedOp "->"
                y <- typeP
                return (FunTy x y))
      <|> tyatom

const :: P SConst
const = SCNat <$> num

sprintOp :: [(Name,Ty)] -> P STerm
sprintOp e = do i <- getPos
                reserved "print"
                str <- option "" stringLiteral
                a <- satom e
                return (SPrint i str a)

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity STerm
binary s f = Ex.Infix (reservedOp s >> return (SBinaryOp NoPos f))

table :: [[Operator String () Identity STerm]]
table = [[binary "+" Add Ex.AssocLeft,
          binary "-" Sub Ex.AssocLeft]]

sexpr :: [(Name,Ty)]-> P STerm
sexpr e = Ex.buildExpressionParser table (stm e)


satom :: [(Name,Ty)]->P STerm
satom e = (flip SConst <$> const <*> getPos)
       <|> flip Sv <$> var <*> getPos
       <|> parens (sexpr e)
       <|> sprintOp e

-- parsea un par (variable : tipo)

binding :: [(Name,Ty)]-> P (Name, Ty)
binding e = do v <- var
               reservedOp ":"
               ty <- parseTy e
               return (v, ty)



binder :: [(Name, Ty)] -> P (Name, Ty)
binder e = parens $ binding e


parseTy :: [(Name,Ty)] -> P Ty
parseTy e = do tyVar <- varST
               case lookup tyVar e of
                     Nothing -> error "No existe"
                     Just t -> return t
                <|> typeP

sletexp :: [(Name,Ty)] -> P STerm
sletexp e = do
  i <- getPos
  reserved "let"
  name <- var
  ls <- many (binder e) -- Agregamos para que parsee sin parentesis
  reservedOp ":"
  retty <- parseTy e
  reservedOp "="
  def <- sexpr e
  let rec = case def of
              SFix {} -> True
              _ -> False
  reserved "in"
  SLet i rec name ls retty def <$> sexpr e

sfix :: [(Name,Ty)] -> P STerm
sfix e = do i <- getPos
            reserved "fix"
            (f, fty) <- binder e
            ls <- many $ binder e
            reservedOp "->"
            SFix i f fty ls <$> sexpr e

slam :: [(Name,Ty)] -> P STerm
slam e = do i <- getPos
            reserved "fun"
            ls <- many $ binder e
            reservedOp "->"
            Slam i ls <$> sexpr e


-- Nota el parser app también parsea un solo atom.
sapp :: [(Name,Ty)]-> P STerm
sapp e = do i <- getPos
            f <- satom e
            args <- many (satom e)
            return (foldl (SApp i) f args)

sifz :: [(Name,Ty)]->P STerm
sifz e= do i <- getPos
           reserved "ifz"
           c <- sexpr e
           reserved "then"
           t <- sexpr e
           reserved "else"
           e <- sexpr e
           return (SIfZ i c t e)



-- | Parser de términos
--tm :: P NTerm
--tm = app <|> lam <|> ifz <|> printOp <|> fix <|> letexp <|> desugar stm

-- Parser de sintactica sugar
stm :: [(Name,Ty)] -> P STerm
stm e = sapp e <|> slam e <|> sifz e <|> sprintOp e <|> sfix e <|> sletexp e


sdecl :: [(Name,Ty)] -> P (SDecl STerm)
sdecl e = do
     i <- getPos
     reserved "let"
     b <- parseRec
     name <- var
     ls <- many (binder e)
     reserved ":"
     ty <- parseTy e
     reservedOp "="
     SDecl i b name ls ty <$> sexpr e
     where parseRec = (reserved "rec" >> return True) <|> return False



sinTypeProgram:: P [(Name,Name)]
sinTypeProgram = many sintype

sintype:: P (Name,Name)
sintype = do
     reserved "type"
     name <- var
     reservedOp "="
     typeName <- varST -- varST permite parsear nombres que representan tipos
     return (name,typeName)



-- | Parser de programas (listas de declaraciones y sinonimos de tipos) 
program :: [(Name,Ty)]-> P [SDecl STerm]
program e = many $ sdecl e
-- program e = do eiths <- many $ sdeclOrSintype e
--                return $ partitionEithers eiths -- separa en 2 listas de acuerdo a si son lefts o rights


sdeclOrSintype :: [(Name,Ty)]->ParsecT String () Identity (Either (SDecl STerm) (Name, Name))
sdeclOrSintype e = try (Left <$> sdecl e) <|> (Right <$> sintype)



-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
-- declOrTm :: P (Either (Decl NTerm) NTerm)
-- declOrTm =  try (Left <$> decl) <|> (Right <$> expr)


declOrSintypeOrStm :: [(Name,Ty)]-> P (Either (Either (SDecl STerm) (Name,Name))  STerm)
declOrSintypeOrStm e =  try (Left <$> sdeclOrSintype e) <|> (Right <$> sexpr e)




-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> [(Name,Ty)]-> STerm
parse s e = case runP (sexpr e) s "" of
                 Right t -> t
                 Left e -> error ("no parse: " ++ show s)
