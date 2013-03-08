
-- TITLE
-- One Hour ML, or How I Learned To Stop Worrying And Write An 
-- ML To Javascript Compiler In About One Hour.
 
-- ABSTRACT
-- In the spirit of [Write Yourself a Scheme in 48hrs](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours),
-- this talk will detail the implementation of a simple compiler for an ML like language, targeting 
-- Javascript.  Topics covered will include the [Parsec](http://www.haskell.org/haskellwiki/Parsec) parser combinator
-- library, the basics of a Hindley/Milner style type inference engine, and the [JMacro](http://www.haskell.org/haskellwiki/Jmacro)
-- quasiquotation language for Javascript generation.  Stopwatches welcome!

-- OUTLINE

-- I. Introduction (10 min) ---------------------------------------------------

--   A. Quick intro, explicitly skip ML history, high level overview

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Arrow

import System.Environment

import Language.Javascript.JMacro 

import qualified Text.Parsec.Token as T
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec hiding ((<|>), many)

import qualified Data.Map  as M
import qualified Data.List as L

--   B. What does the language look like, intro to the AST.  

--     1. Let, Abs, App, vars and literals.
 
data Expr where

    LetExpr :: Sym  -> Expr -> Expr -> Expr
    AppExpr :: Expr -> Expr -> Expr
    AbsExpr :: Sym  -> Expr -> Expr
    VarExpr :: Val  -> Expr
    MatExpr :: Expr -> [(Patt, Expr)] -> Expr

    deriving (Show)

data Patt where

    ValPatt :: Val -> Patt
    ConPatt :: Sym -> [Patt] -> Patt

    deriving (Show)

data Val where

    SymVal  :: Sym -> Val
    LitVal  :: Lit -> Val

    deriving (Show)

newtype Sym = Sym String

    deriving (Show)

data Lit where

    StrLit  :: String -> Lit
    NumLit  :: Double -> Lit
    BoolLit :: Bool   -> Lit

    deriving (Show)

--     2. Simple example programs, also test cases.

samplePrograms :: [String]
samplePrograms = [

    "2.0 + 2.0 == 4.0",

    "let x = 1.0; x + 1.0 == 2.0",

    "(fun x -> x + 4) 2 == 6",

    "let fib = fun n ->\
    \    match n with\
    \    0 -> 0;\
    \    1 -> 1;\
    \    n -> fib (n - 1) + fib (n - 2);;\
    \fib 13",

    "let truely = 4; 4"

    ]

--   C. Compiler architecture

main :: IO ()
main  = do

    source <- head <$> getArgs
    case compile source of
        Left  e  -> print e
        Right js -> putStrLn js

compile :: String -> Either Err String
compile x = parseOhml x >>= typeCheck >>= generateJs >>= toText

toText :: JExpr  -> Either Err String
toText = Right . show . renderJs 

generateJs = Right . toJExpr

unwrap (Right x) = putStrLn x >> putStrLn "----------------------"


-- II. Parsing (20 min) -------------------------------------------------------

--   A. Overview of Parsec design, how Parser a forms an applicative functor
--      and why that's interesting.

--     1. Natural similarity, left to right data constructor mirrors parsing

parseOhml :: String -> Either Err Expr
parseOhml = left (Err . show) . parse (exprP <* eof) "Parsing OHML" 

newtype Err = Err String deriving Show

--   B. Language and Token modules
--      We don't need reservedOps because there are no user operators!

ops = [ [ "^" ]
      , [ "*", "/" ]
      , [ "+", "-" ]
      , [ "<", "<=", ">=", ">", "==", "!=" ]
      , [ "&&", "||" ] ]

ohmlDef = emptyDef {
    T.reservedNames = [ "let", "true", "false", "fun", "match", "with" ]
}

T.TokenParser { .. } = T.makeTokenParser ohmlDef

--   C. A Parser for OHML

--     1. The Literal parser is a simple parser which generates Lit values.
--        Here, `stringLiteral` and `float` come from `T.TokenParser`.

litP :: Parser Lit
litP =

    stringL <|> numL <|> boolL

    where
        stringL = StrLit  <$> stringLiteral
        boolL   = BoolLit <$> (true <|> false)
        numL    = NumLit . toDouble <$> naturalOrFloat

        true  = reserved "true"  *> return True
        false = reserved "false" *> return False

        toDouble (Left i)  = fromInteger i
        toDouble (Right f) = f

--     2. The `Sym` and `Val` parsers

symP :: Parser Sym
symP = Sym <$> identifier

valP :: Parser Val
valP = (SymVal <$> symP) <|> (LitVal <$> litP)

--     3. Pattern parser introduces `parens`

pattP :: Parser Patt
pattP =

    valPattP <|> conPattP <|> parens conPatsP

    where
        valPattP = ValPatt <$> valP
        conPattP = flip ConPatt [] <$> symP
        conPatsP = ConPatt <$> symP <*> many pattP <|> pattP

--     4. The `Expr` parser makes use of a number of `T.TokenParser` lexers.

exprP :: Parser Expr
exprP =

    letExprP
        <|> absExprP 
        <|> matExprP 
        <|> appExprP 
        <|> valExprP 
        <|> parens exprP

    where
        absExprP =
            reserved "fun"
                >>  AbsExpr
                <$> symP
                <*  reservedOp "->"
                <*> exprP          

        matExprP =
            reserved "match"
                >>  MatExpr
                <$> exprP
                <*  reserved "with"
                <*> many1 (try caseP)
            where
                caseP =
                    (,) <$> pattP <*  reservedOp "->" <*> exprP <* semi

        letExprP =
            reserved "let" 
                >>  LetExpr
                <$> symP
                <*  reservedOp "="
                <*> exprP
                <*  semi
                <*> exprP

        valExprP = VarExpr <$> valP

        appExprP =
            buildExpressionParser (map (map ix) ops) termP
            where
                termP =
                    (valExprP <|> parens exprP) `chainl1` return AppExpr
                op = 
                    (AppExpr .) . AppExpr . VarExpr . SymVal . Sym
                ix =
                    flip Infix AssocLeft 
                        . uncurry (*>) 
                        . (reservedOp &&& return . op) 



-- III. Type Inference (20 min) -----------------------------------------------

--   A. Overview of the algorithm, a TypeCheck monad
--         (TODO) may have to drop polymorphic types to save time.

typeCheck  :: Expr   -> Either Err Expr
typeCheck   = Right

--   B. Instantiation of type variables

--   C. Unification

--   D. Generalization

-- IV. Code Generation (25 minutes)

--   A. Introduction to JMacro

--     1. Features overview, quasiquotation

--     2. What you get for free by using JMacro/Javascript.

--   B. Marshalling the OHML AST into JExprs

instance ToJExpr Val where

    toJExpr (SymVal s) = toJExpr s
    toJExpr (LitVal l) = toJExpr l

instance ToJExpr Sym where

    toJExpr (Sym x) = ref x

instance ToJExpr Lit where

    toJExpr (StrLit s)  = toJExpr s
    toJExpr (BoolLit b) = toJExpr b
    toJExpr (NumLit n)  = toJExpr n


--   D. Hygenic introduction of variables

intro :: (ToJExpr a) => String -> (JExpr -> a) -> Expr -> JExpr
intro sym f expr = [jmacroE| 

    function(arg) {
        `(DeclStat (StrI sym) Nothing)`;
        `(ref sym)` = `(f arg)`;
        return `(expr)`;
    }

|]

instance ToJExpr Expr where

    toJExpr (isInline -> Just (x, o, y)) =

        InfixExpr o (toJExpr x) (toJExpr y)

    toJExpr (AppExpr f x) = 

        [jmacroE| `(f)`(`(x)`) |]

    toJExpr (AbsExpr (Sym sym) ex) = 

        intro sym id ex

    toJExpr (VarExpr v) =

        toJExpr v

    toJExpr (LetExpr (Sym sym) ex expr) = [jmacroE| 

        `(intro sym (const ex) expr)`()

    |]

    toJExpr (MatExpr val ((c, exp):cases)) = [jmacroE|

        (function() {
            if (`(Match val c)`)
                return `(exp)` 
            else
                return `(MatExpr val cases)`;
        })()

    |]

    toJExpr (MatExpr val []) = [jmacroE|

        (function() {
            throw new Exception("Pattern Match Exhausted");
        })()

    |]

data Match = Match Expr Patt deriving (Show)

ref :: String -> JExpr
ref = ValExpr . JVar . StrI

isInline :: Expr -> Maybe (Expr, String, Expr)
isInline (AppExpr (AppExpr (VarExpr (SymVal (Sym o))) x) y) 
    | o `elem` concat ops  = Just (x, o, y)
isInline _ = Nothing

instance ToJExpr Match where

    toJExpr (Match val (ValPatt (LitVal l))) =

        [jmacroE| `(l)` == `(val)` |]

    toJExpr (Match val (ValPatt (SymVal (Sym s)))) = [jmacroE|

        (function() {
            this[`(s)`] = `(val)`;
            return true;
        })() 

    |]

 

-- V. Wrap up, run the samples from the intro.

-- LICENSE
-- Copyright (c) 2013 Andrew Stein

-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

