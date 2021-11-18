{-|
Module      : Parse
Description : Define un parser de términos FD40 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (stm, Parse.parse, sdecl, runP, P, program, declOrStm,typeP) where

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
import Data.Text.Internal.Fusion.Common (concat)
import qualified Data.Foldable

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
tyatom = do reserved "Nat" >> return NatTy
         <|> (varST >>= \n->return $ SinTy n)
         <|> parens typeP

typeP :: P Ty
typeP = try (do x <- tyatom
                reservedOp "->"
                FunTy x <$> typeP)
        <|> tyatom

const :: P SConst
const = SCNat <$> num

sprintOp :: P STerm
sprintOp = do i <- getPos
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
satom = (flip SConst <$> const <*> getPos)
       <|> flip Sv <$> var <*> getPos
       <|> parens sexpr
       <|> sprintOp

-- parsea un par (variable : tipo)

binding :: P [(Name, Ty)]
binding = do v <- many var
             reservedOp ":"
             ty <- typeP
             return [(n,ty)| n <- v]
             



binder ::P [(Name, Ty)]
binder = parens binding



sletexp :: P STerm
sletexp = do
  i <- getPos
  reserved "let"
  name <- var
  ls <- many binder -- Agregamos para que parsee sin parentesis
  let ls' = Data.Foldable.concat ls 
  reservedOp ":"
  retty <- typeP
  reservedOp "="
  def <- sexpr
  let rec = case def of
              SFix {} -> True
              _ -> False
  reserved "in"
  SLet i rec name ls' retty def <$> sexpr

sfix :: P STerm
sfix   = do i <- getPos
            reserved "fix"
            [(f, fty)] <- binder
            ls <- many binder
            let ls' = Data.Foldable.concat ls 
            reservedOp "->"
            SFix i f fty ls' <$> sexpr

slam :: P STerm
slam = do i <- getPos
          reserved "fun"
          ls <- many binder
          let ls' = Data.Foldable.concat ls 
          reservedOp "->"
          Slam i ls' <$> sexpr


-- Nota el parser app también parsea un solo atom.
sapp :: P STerm
sapp = do i <- getPos
          f <- satom
          args <- many satom
          return (foldl (SApp i) f args)

sifz :: P STerm
sifz  = do i <- getPos
           reserved "ifz"
           c <- sexpr
           reserved "then"
           t <- sexpr
           reserved "else"
           SIfZ i c t <$> sexpr


declOrStm ::  P (Either (SDecl STerm) STerm)
declOrStm =  try (Left <$> sdeclOrSintype) <|> (Right <$> sexpr)


-- Parser de sintactica sugar
stm :: P STerm
stm = sapp  <|> slam  <|> sifz <|> sprintOp <|> sfix  <|> sletexp


sdecl :: P (SDecl STerm)
sdecl = do
     i <- getPos
     reserved "let"
     b <- parseRec
     name <- var
     ls <- many binder
     let ls' = Data.Foldable.concat ls 
     reserved ":"
     ty <-  typeP
     reservedOp "="
     SDecl i b name ls' ty <$> sexpr
     where parseRec = (reserved "rec" >> return True) <|> return False



sintype::P (SDecl a)
sintype = do reserved "type"
             name <- var
             reservedOp "="
             value <- typeP
             return $ SType name value



-- | Parser de programas (listas de declaraciones y sinonimos de tipos) 
program :: P ([SDecl STerm])
--program e = many $ sdecl e
program = many $ sdeclOrSintype
             -- separa en 2 listas de acuerdo a si son lefts o rights



sdeclOrSintype :: ParsecT String () Identity (SDecl STerm)
sdeclOrSintype = try (sdecl <|> sintype)







-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse ::String ->  STerm
parse s = case runP sexpr s "" of
                Right t -> t
                Left e -> error ("no parse: " ++ show s)
