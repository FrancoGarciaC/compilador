{-|
Module      : Parse
Description : Define un parser de términos FD40 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (stm, Parse.parse, sdecl, runP, P, program, declOrStm) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP,parse)
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Expr (Operator, Assoc)
import Control.Monad.Identity (Identity)
import Elab (buildType)

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

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P Ty
tyatom = (reserved "Nat" >> return NatTy)
         <|> parens typeP

typeP :: P Ty
typeP = try (do
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (FunTy x y))
      <|> tyatom

const :: P SConst
const = SCNat <$> num

sprintOp :: P STerm
sprintOp = do
  i <- getPos
  reserved "print"
  str <- option "" stringLiteral
  a <- satom
  return (SPrint i str a)

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity STerm
binary s f = Ex.Infix (reservedOp s >> return (SBinaryOp NoPos f))

table :: [[Operator String () Identity STerm]]
table = [[binary "+" Add Ex.AssocLeft,
          binary "-" Sub Ex.AssocLeft]]

sexpr :: P STerm
sexpr = Ex.buildExpressionParser table stm


satom :: P STerm
satom =     (flip SConst <$> const <*> getPos)
       <|> flip Sv <$> var <*> getPos
       <|> parens sexpr
       <|> sprintOp

-- parsea un par (variable : tipo)
binding :: P (Name, Ty)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)

binder = parens binding

sletexp :: P STerm
sletexp = do
  i <- getPos
  reserved "let"
  name <- var
  ls <- many binder -- Agregamos para que parsee sin parentesis
  reservedOp ":"
  retty <- typeP
  reservedOp "="
  def <- sexpr
  let rec = case def of
              (SFix _ _ _ _ _) -> True
              _ -> False
  reserved "in"
  body <- sexpr
  return (SLet i rec name ls retty def body)

sfix :: P STerm
sfix = do i <- getPos
          reserved "fix"
          (f, fty) <- binder
          ls <- many binder
          reservedOp "->"
          t <- sexpr
          return (SFix i f fty ls t)

slam :: P STerm
slam = do i <- getPos
          reserved "fun"
          ls <- many binder
          reservedOp "->"
          t <- sexpr
          return (Slam i ls t)


-- Nota el parser app también parsea un solo atom.
sapp :: P STerm
sapp = (do i <- getPos
           f <- satom
           args <- many satom
           return (foldl (SApp i) f args))

sifz :: P STerm
sifz = do i <- getPos
          reserved "ifz"
          c <- sexpr
          reserved "then"
          t <- sexpr
          reserved "else"
          e <- sexpr
          return (SIfZ i c t e)



-- | Parser de términos
--tm :: P NTerm
--tm = app <|> lam <|> ifz <|> printOp <|> fix <|> letexp <|> desugar stm

-- Parser de sintactica sugar
stm :: P STerm
stm = sapp <|> slam <|> sifz <|> sprintOp <|> sfix <|> sletexp


sdecl :: P (SDecl STerm)
sdecl = do
     i <- getPos
     reserved "let"
     b <- parseRec    
     name <- var
     ls <- many binder
     reserved ":"     
     ty <- typeP        
     reservedOp "="
     SDecl i b name ls ty <$> sexpr

     where parseRec = (reserved "rec" >> return True) <|> return False


-- | Parser de programas (listas de declaraciones) 
program :: P [SDecl STerm]
program = many sdecl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
-- declOrTm :: P (Either (Decl NTerm) NTerm)
-- declOrTm =  try (Left <$> decl) <|> (Right <$> expr)


declOrStm :: P (Either (SDecl STerm) STerm)
declOrStm =  try (Left <$> sdecl) <|> (Right <$> sexpr)




-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP sexpr s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
