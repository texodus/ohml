
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
import Control.Monad.State
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
typeCheck expr =
    fmap (const expr . fst)
         . flip runStateT (nullSubst, 0)
         . exprCheck prelude
         $ expr

prelude :: [Assump]
prelude = [ 
    "==" :>: Forall [Star] (TGen 0 `fn` TGen 0 `fn` TGen 0),
    "+"  :>: Forall []  (tDouble `fn` tDouble `fn` tDouble),
    "-"  :>: Forall []  (tDouble `fn` tDouble `fn` tDouble)
    ]   

data Kind  = Star | Kfun Kind Kind deriving (Eq, Show)

data Type  = TVar Tyvar | TCon Tycon | TAp  Type Type | TGen Int
               deriving (Eq, Show)
 
data Tyvar = Tyvar String Kind
             deriving (Eq, Show)

data Tycon = Tycon String Kind
             deriving (Eq, Show)

tString  = TCon (Tycon "String" Star)
tBool    = TCon (Tycon "Boolean" Star)
tDouble  = TCon (Tycon "Double" Star)
tList    = TCon (Tycon "[]" (Kfun Star Star))
tArrow   = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))

infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TAp (TAp tArrow a) b

list       :: Type -> Type
list t      = TAp tList t

class HasKind t where
   kind :: t -> Kind
instance HasKind Tyvar where
   kind (Tyvar v k) = k
instance HasKind Tycon where
   kind (Tycon v k) = k
instance HasKind Type where
   kind (TCon tc) = kind tc
   kind (TVar u)  = kind u
   kind (TAp t _) = case (kind t) of
                      (Kfun _ k) -> k

type Subst  = [(Tyvar, Type)]

nullSubst  :: Subst
nullSubst   = []

(+->)      :: Tyvar -> Type -> Subst
u +-> t     = [(u, t)]

class Types t where
    apply :: Subst -> t -> t
    tv    :: t -> [Tyvar]

instance Types Type where
    apply s (TVar u)  = case lookup u s of
                         Just t  -> t
                         Nothing -> TVar u
    apply s (TAp l r) = TAp (apply s l) (apply s r)
    apply s t         = t
 
    tv (TVar u)  = [u]
    tv (TAp l r) = tv l `L.union` tv r
    tv t         = []

instance Types a => Types [a] where
    apply s = map (apply s)
    tv      = L.nub . concat . map tv    

infixr 4 @@
(@@)       :: Subst -> Subst -> Subst
s1 @@ s2    = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

mgu     :: Monad m => Type -> Type -> m Subst
varBind :: Monad m => Tyvar -> Type -> m Subst

mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
                               s2 <- mgu (apply s1 r) (apply s1 r')
                               return (s2 @@ s1)
mgu (TVar u) t        = varBind u t
mgu t (TVar u)        = varBind u t
mgu (TCon tc1) (TCon tc2)
           | tc1==tc2 = return nullSubst
mgu t1 t2             = fail ("types do not unify;\n" ++ show t1 ++ " and " ++ show t2)

varBind u t | t == TVar u      = return nullSubst
            | u `elem` tv t    = fail "occurs check fails"
            | kind u /= kind t = fail "kinds do not match"
            | otherwise        = return (u +-> t)

data Scheme = Forall [Kind] Type
              deriving Eq

instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    tv (Forall ks qt)      = tv qt

quantify      :: [Tyvar] -> Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where vs' = [ v | v <- tv qt, v `elem` vs ]
       ks  = map kind vs'
       s   = zip vs' (map TGen [0..])

toScheme      :: Type -> Scheme
toScheme     = Forall []

data Assump = String :>: Scheme

instance Types Assump where
    apply s (i :>: sc) = i :>: (apply s sc)
    tv (i :>: sc)      = tv sc

find                 :: Monad m => String -> [Assump] -> m Scheme
find i []             = fail ("unbound identifier: " ++ i)
find i ((i':>:sc):as) = if i==i' then return sc else find i as

-- type TI a = TI (Either Err(Subst -> Int -> (Subst, Int, a)))

type TI a = StateT (Subst, Int) (Either Err) a

getSubst   :: TI Subst
getSubst    = fst <$> get

unify      :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst   :: Subst -> TI ()
extSubst s' = do
    (s, i) <- get
    put (s' @@ s, i)

newTVar    :: Kind -> TI Type
newTVar k   = do
    (s, i) <- get
    put (s, i + 1)
    return (TVar (Tyvar ("tvar_" ++ show i) k))

freshInst               :: Scheme -> TI Type
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

class Instantiate t where
  inst  :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst ts t         = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

litCheck :: Lit -> Type
litCheck (StrLit _)  = tString
litCheck (NumLit _)  = tDouble
litCheck (BoolLit _) = tBool

pattCheck :: Patt -> TI ([Assump], Type)

pattCheck (ValPatt (SymVal (Sym s))) = do
    t <- newTVar Star
    return ([ s :>: Forall [] t ], t)

pattCheck (ValPatt (LitVal l)) = do
    return ([], litCheck l)

exprCheck :: [Assump] -> Expr -> TI Type

exprCheck as (LetExpr (Sym sym) val expr) = do
    symT <- newTVar Star
    valT <- exprCheck ((sym :>: Forall [] symT) : as) val
    unify valT symT
    exprCheck ((sym :>: Forall [] symT) : as) expr

exprCheck as (AppExpr f x) = do
    fT   <- exprCheck as f
    xT   <- exprCheck as x
    appT <- newTVar Star
    unify (xT `fn` appT) fT
    return appT

exprCheck as (AbsExpr (Sym sym) expr) = do
    x   <- newTVar Star
    res <- exprCheck ((sym :>: Forall [] x) : as) expr
    return (x `fn` res)


exprCheck as (VarExpr (SymVal (Sym sym))) =
    find sym as >>= freshInst

exprCheck as (VarExpr (LitVal l)) =
    return (litCheck l)

exprCheck as (MatExpr expr ((patt, res):[])) = do
    exprT <- exprCheck as expr
    (pattAs, pattT) <- pattCheck patt
    unify exprT pattT
    exprCheck (pattAs ++ as) res

exprCheck as (MatExpr expr ((patt, res):es)) = do
    exprT <- exprCheck as expr
    (pattAs, pattT) <- pattCheck patt
    unify exprT pattT
    resT  <- exprCheck (pattAs ++ as) res
    esT   <- exprCheck as (MatExpr expr es)
    unify resT esT
    return resT
    




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

    toJExpr (MatExpr val ((patt, expr):cases)) = [jmacroE|

        (function() {
            var scope = this;
            if (`(Match val patt scope)`)
                return `(expr)`
            else
                return `(MatExpr val cases)`;
        })()

    |]

    toJExpr (MatExpr val []) = [jmacroE|

        (function() {
            throw new Exception("Pattern Match Exhausted");
        })()

    |]

data Match = Match Expr Patt JExpr deriving (Show)

ref :: String -> JExpr
ref = ValExpr . JVar . StrI

isInline :: Expr -> Maybe (Expr, String, Expr)
isInline (AppExpr (AppExpr (VarExpr (SymVal (Sym o))) x) y) 
    | o `elem` concat ops  = Just (x, o, y)
isInline _ = Nothing

instance ToJExpr Match where

    toJExpr (Match val (ValPatt (LitVal l)) _) =

        [jmacroE| `(l)` == `(val)` |]

    toJExpr (Match val (ValPatt (SymVal (Sym s))) scope) = [jmacroE|

        (function() {
            `(scope)`[`(s)`] = `(val)`;
            return true;
        })()

    |]

 

-- V. Wrap up, run the samples from the intro.

-- LICENSE
-- Copyright (c) 2013 Andrew Stein

-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

