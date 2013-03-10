-------------------------------------------------------------------------------

-- One Hour ML, or How I Learned To Stop Worrying And Write An 
-- ML To Javascript Compiler In About One Hour.

-------------------------------------------------------------------------------

-- * [Write Yourself a Scheme in 48hrs]
--   (http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours),

-- * [Parsec]
--   (http://www.haskell.org/haskellwiki/Parsec)


-- * [JMacro]
--   (http://www.haskell.org/haskellwiki/Jmacro)

-- * [Typing Haskell in Haskell]
--   (]http://web.cecs.pdx.edu/~mpj/thih/)

-- I. -------------------------------------------------------------------------

-- Quick intro, explicitly skip ML history, high level overview 

-------------------------------------------------------------------------------

-- There are some extensions necessary - there necessity will be described 
-- inline.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

-------------------------------------------------------------------------------

-- There is one module.  We'll need the environment to take input, and a
-- handful of elements from the `containers` and `mtl` libraris. 

module Main where

import System.Environment

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Arrow

import qualified Data.Map  as M
import qualified Data.List as L

-------------------------------------------------------------------------------

-- We'll also be using Parsec for parsing, and JMacro for code generation

import qualified Text.Parsec.Token as T
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec hiding ((<|>), many)

import Language.Javascript.JMacro

-- I.B. -----------------------------------------------------------------------

-- What does the language look like, intro to the AST. 

-------------------------------------------------------------------------------

-- The language itself will be a simple expression language

samplePrograms :: [String]
samplePrograms = [

-------------------------------------------------------------------------------

-- Simple types and infix operators will compile to their identical 
-- underlying implementations.  Yes, that means there are only `Num`s!

    "   2 + 2.0 == 4.0   ",

-------------------------------------------------------------------------------

-- You can introduce symbols with the `let` keyword.
-- (Note: `\` is Haskell's multiline syntax)

    "   let x = 1.0;     \
    \   x + 1.0 == 2.0   ",

-------------------------------------------------------------------------------

-- Functions - we'll refer to this as "Abstraction" to sound cool!
-- Also pictured: prefix application.

    "   (fun x -> x + 4) 2 == 6   ",

-------------------------------------------------------------------------------

-- Pattern Matching (tm) FUN!

    "   let fib = fun n ->                     \
    \       match n with                       \
    \       0 -> 0;                            \
    \       1 -> 1;                            \
    \       n -> fib (n - 1) + fib (n - 2);;   \
    \   fib 1                                  ",

-------------------------------------------------------------------------------

-- User defined algebraic data type constructors.
-- (TODO Address GADTs)

    "   data Cons = forall a => a -> List a -> List a;   \
    \   data Nil  = forall a => List a;                  \
    \                                                    \
    \   let length = fun n ->                            \
    \       match n with                                 \
    \       Nil -> 0;                                    \
    \       (Cons _ xs) -> 1 + length xs;;               \
    \                                                    \
    \   length (Cons 1 (Cons 2 Nil)) == 2                ",

-------------------------------------------------------------------------------

-- And of course, our language will not do dumb things while parsing    

    "   let letly = 4; (let func = fun x -> x + ((2)); func letly)   ",

-------------------------------------------------------------------------------

-- ... or while type checking

    "   4 + \"whoops\"   ",

    "   match 3 + 3 with \"gotcha\" -> false   ",

-------------------------------------------------------------------------------

-- ... or while generating javascript code.

    "   let x = 3;                \
    \   let f = fun y -> y + x;   \
    \   let x = 2;                \
    \   f 3 == 6                  "

-------------------------------------------------------------------------------

-- Other examples should be inserted here ...

    ]

-- I.C. -----------------------------------------------------------------------

-- Compiler Architecture

-------------------------------------------------------------------------------

-- Compiler "Architecture"

-------------------------------------------------------------------------------

-- The compiler will be a simple program that takes a file name, and
-- emits Javascript to stdout.

main :: IO ()
main  = do

    source <- head <$> getArgs
    case compile source of
        Left  e  -> print e
        Right js -> putStrLn js

-------------------------------------------------------------------------------

-- The structure of compilation is quite simple, expressed as a simple
-- function composition.  

compile :: String -> Either Err String
compile = parseOhml >=> typeCheck >=> generateJs >=> toText

newtype Err = Err String deriving Show


-- II. --------------------------------------------------------------------

-- The Abstract Syntax Tree, or AST in the 'biz.

-------------------------------------------------------------------------------

-- The Abstract Syntax Tree, or AST in the 'biz.
-- We also say 'biz in da 'biz.

-------------------------------------------------------------------------------

-- The 10,000 ft view of an an OHML program is a simple expression. 
-- Here, `AbsExpr` is an Abstraction, and `AppExpr` is an Application.
-- (Note: We use GADT syntax solely for clarity - even though
-- this is not necessary)

data Expr where

    LetExpr :: Bind -> Expr -> Expr
    AppExpr :: Expr -> Expr -> Expr
    AbsExpr :: Sym  -> Expr -> Expr
    VarExpr :: Val  -> Expr                     -- TODO spell me correktly!
    MatExpr :: Expr -> [(Patt, Expr)] -> Expr

    deriving (Show)

-------------------------------------------------------------------------------

-- TODO Describe symbol and type bindings

data Bind where

    SymBind :: Sym -> Expr   -> Bind
    TypBind :: Typ -> TypSch -> Bind

    deriving (Show)

-------------------------------------------------------------------------------

-- TODO describe the type signature AST

data Typ where

    TypSym :: String -> Typ
    TypVar :: String -> Typ
    TypApp :: Typ -> Typ -> Typ

    deriving (Show)  

data TypSch where

    TypSch :: [Sym] -> Typ -> TypSch

    deriving (Show)
    
-------------------------------------------------------------------------------

-- Patterns are either `Val` or `Con` (which is really a decon, amirite?).
-- Note we do not distinguish between literal and symbol matching, 
-- because this is captured in the definition of `Val`

data Patt where

    ValPatt :: Val -> Patt
    ConPatt :: Typ -> [Patt] -> Patt

    deriving (Show)

-------------------------------------------------------------------------------

-- ... which looks like this.

data Val where

    SymVal  :: Sym -> Val
    LitVal  :: Lit -> Val
    ConVal  :: Typ -> Val

    deriving (Show)

-------------------------------------------------------------------------------

-- Symbols and literals, yada yada yada.

newtype Sym = Sym String

    deriving (Show)

data Lit where

    StrLit  :: String -> Lit
    NumLit  :: Double -> Lit

    deriving (Show)



-- III. -----------------------------------------------------------------------

-- Parsing With Prejudice 

-------------------------------------------------------------------------------

-- ... goes to Hackage, downloades Parsec ...

-------------------------------------------------------------------------------

-- What is a Parser?  Well, this is a pretty good definition, which allows us
-- to define some combinators that are also `MyParser`

type MyParser a = String -> Either Err (a, String)

-------------------------------------------------------------------------------

-- TODO Explain `Parser` and `GenParser`

-------------------------------------------------------------------------------

-- TODO Applicative Functor, Functor & Monad members.

-------------------------------------------------------------------------------

-- With this in mind, we can define the parser simply with the `parse`
-- from Parsec.

parseOhml :: String -> Either Err Expr
parseOhml = left (Err . show) . parse grammar "Parsing OHML" 

    where
        grammar = spaces *> exprP <* eof

-------------------------------------------------------------------------------

-- TODO Explain combinators `*>` and `<*`

-------------------------------------------------------------------------------

-- There is some static info we need to define about OHML.  The language
-- keywords ...

keywords = [ "let", "fun", "match", "with", "data" ]

-- ... and operators, arranged in precedence order.

ops = [ [ "^" ]
      , [ "*", "/" ]
      , [ "+", "-" ]
      , [ "<", "<=", ">=", ">", "==", "!=" ]
      , [ "&&", "||" ] ]

-------------------------------------------------------------------------------

-- Parsec provides lexing for free

ohmlDef = emptyDef {
    T.reservedNames   = keywords,
    T.reservedOpNames = L.concat ops
}

T.TokenParser { .. } = T.makeTokenParser ohmlDef

-------------------------------------------------------------------------------

-- The simplest parsers are for `Sym` and `Val`
-- (TODO explain `<$>`)

symP :: Parser Sym
symP = Sym <$> identifier

-- (TODO explain `<|>`)

valP :: Parser Val
valP = (SymVal <$> symP) <|> (LitVal <$> litP) <|> (ConVal <$> typSymP)

-------------------------------------------------------------------------------

-- So the next simplest parser we can define is the parser for literals
-- The Literal parser is a simple parser which generates Lit values.
-- TODO explain `<|>` and `try`

litP :: Parser Lit
litP = stringL <|> numL

-------------------------------------------------------------------------------

-- Here, `stringLiteral` and `float` come from `T.TokenParser`.
-- TODO explain `<$>`

    where
        stringL = StrLit  <$> stringLiteral
        numL    = NumLit . toDouble <$> naturalOrFloat

-- with the help of some helpers.

        toDouble (Left i)  = fromInteger i
        toDouble (Right f) = f

-------------------------------------------------------------------------------

-- Pattern parser introduces `parens` and `many`

pattP :: Parser Patt
pattP =

    valPattP <|> conPattP <|> parens conPatsP

    where
        valPattP = ValPatt <$> valP
        conPattP = flip ConPatt [] <$> typP
        conPatsP = ConPatt <$> typP <*> many pattP <|> pattP

-------------------------------------------------------------------------------

-- The `Expr` parser makes use of a number of `T.TokenParser` lexers.

exprP :: Parser Expr
exprP =

    letExprP
        <|> typExprP
        <|> absExprP 
        <|> matExprP 
        <|> appExprP 

-- and we obviously need to define all of these 

    where

-------------------------------------------------------------------------------

-- (TODO explain `reserved`)

        absExprP =
            reserved "fun"
                >>  AbsExpr
                <$> symP
                <*  reservedOp "->"
                <*> exprP
                <?> "Abstraction"        

-------------------------------------------------------------------------------

        matExprP =
            reserved "match"
                >>  MatExpr
                <$> exprP
                <*  reserved "with"
                <*> many1 (try caseP)
                <?> "Match Expression"

            where
                caseP =
                    (,) <$> pattP <*  reservedOp "->" <*> exprP <* semi

-------------------------------------------------------------------------------

        letExprP =
            reserved "let" 
                >>  (LetExpr .) . SymBind
                <$> symP
                <*  reservedOp "="
                <*> exprP
                <*  semi
                <*> exprP
                <?> "Let Expression"

-------------------------------------------------------------------------------

        typExprP =
            reserved "data" 
                >>  (LetExpr .) . TypBind
                <$> typP
                <*  reservedOp "="
                <*> typSchP
                <*  semi
                <*> exprP
                <?> "Type Expression"

-------------------------------------------------------------------------------

        appExprP =

            buildExpressionParser (ifx opConst ops) termP <?> "Application"

            where
                valExprP =
                    VarExpr <$> valP <?> "Value"
                termP =
                    (valExprP <|> parens exprP) `chainl1` return AppExpr

                opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym 

-------------------------------------------------------------------------------

-- TODO explain this crap

ifx op =
    map $ map
        $ flip Infix AssocLeft 
        . uncurry (*>) 
        . (reservedOp &&& return . op) 

typP :: Parser Typ
typP = typAppP <?> "Type Symbol"

    where
        typAppP = buildExpressionParser (ifx fnConst [[ "->" ]]) termP
        fnConst = (TypApp .) . TypApp . TypSym
        termP   = (typVarP <|> typSymP) `chainl1` return TypApp
        typVarP = TypVar <$> identifier <?> "Type Variable"

typSchP :: Parser TypSch
typSchP =
    reserved "forall"
        >>  TypSch . (:[]) . Sym
        <$> identifier
        <*  reservedOp "=>"
        <*> typP
        <?> "Type Scheme"

typSymP :: Parser Typ
typSymP = (TypSym .) . (:) <$> upper <*> identifier

-------------------------------------------------------------------------------

-- ... and that's it!

-- III. -----------------------------------------------------------------------

-- Type Inference

-------------------------------------------------------------------------------

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
    "+"  :>: Forall []     (tDouble `fn` tDouble `fn` tDouble),
    "-"  :>: Forall []     (tDouble `fn` tDouble `fn` tDouble)
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
            | otherwise        = return [(u, t)]

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

data Assump = String :>: Scheme

instance Types Assump where
    apply s (i :>: sc) = i :>: (apply s sc)
    tv (i :>: sc)      = tv sc

find                 :: Monad m => String -> [Assump] -> m Scheme
find i []             = fail ("unbound identifier: " ++ i)
find i ((i':>:sc):as) = if i==i' then return sc else find i as

type TypeCheck a = StateT (Subst, Int) (Either Err) a

getSubst   :: TypeCheck Subst
getSubst    = fst <$> get

unify      :: Type -> Type -> TypeCheck ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

extSubst   :: Subst -> TypeCheck ()
extSubst s' = do
    (s, i) <- get
    put (s' @@ s, i)

newTVar    :: Kind -> TypeCheck Type
newTVar k   = do
    (s, i) <- get
    put (s, i + 1)
    return (TVar (Tyvar ("tvar_" ++ show i) k))

freshInst               :: Scheme -> TypeCheck Type
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

class Instantiate t where
  inst  :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst ts t         = t

--instance Instantiate a => Instantiate [a] where
--  inst ts = map (inst ts)

litCheck :: Lit -> Type
litCheck (StrLit _)  = tString
litCheck (NumLit _)  = tDouble

pattCheck :: Patt -> TypeCheck ([Assump], Type)

pattCheck (ValPatt (SymVal (Sym s))) = do
    t <- newTVar Star
    return ([ s :>: Forall [] t ], t)

pattCheck (ValPatt (LitVal l)) = do
    return ([], litCheck l)

exprCheck :: [Assump] -> Expr -> TypeCheck Type

exprCheck as (LetExpr (SymBind (Sym sym) val) expr) = do
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

generateJs = Right . toJExpr

--   B. Marshalling the OHML AST into JExprs

instance ToJExpr Val where

    toJExpr (SymVal s) = toJExpr s
    toJExpr (LitVal l) = toJExpr l

instance ToJExpr Sym where

    toJExpr (Sym x) = ref x

instance ToJExpr Lit where

    toJExpr (StrLit s)  = toJExpr s
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

    toJExpr (LetExpr (SymBind (Sym sym) ex) expr) = [jmacroE| 

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


-- UTypeCheckLS


toText :: JExpr  -> Either Err String
toText = Right . show . renderJs  


unwrap (Right x) = putStrLn x >> putStrLn "----------------------"
unwrap (Left x)  = putStrLn (show x) >> putStrLn "----------------------"


-- V. Wrap up, run the samples from the intro.

-- LICENSE
-- Copyright (c) 2013 Andrew Stein

-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTypeCheckES OF MERCHANTABILITY, FITNESS FOR A PARTypeCheckCULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTypeCheckON OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTypeCheckON WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

