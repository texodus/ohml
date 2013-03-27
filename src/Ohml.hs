
-- One Hour ML, or How I Learned To Stop Worrying And Write An 
-- ML To Javascript Compiler In About One Hour.

------------------------------------------------------------------------------

-- (1) [Write Yourself a Scheme in 48hrs]
--     (http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours),

-- (2) [Parsec]
--     (http://www.haskell.org/haskellwiki/Parsec)

-- (3) [JMacro]
--     (http://www.haskell.org/haskellwiki/Jmacro)

-- (4) [Typing Haskell in Haskell]
--     (http://web.cecs.pdx.edu/~mpj/thih/)

-- (5) [OHML]
--     (https://github.com/texodus/ohml)

-- (6) [Forml]
--     (http://http://texodus.github.com/forml)

------------------------------------------------------------------------------

--       --==>  http://http://texodus.github.com/forml/  <==--

--       --==>  http://http://texodus.github.com/forml/  <==--


--                 Learn Forml, or I'll claw up your furniture!
--          /\   / 
--      )  ( ')  
--     (  /  )
--      \(__)|


--       --==>  http://http://texodus.github.com/forml/  <==--

--       --==>  http://http://texodus.github.com/forml/  <==--

------------------------------------------------------------------------------

-- I. Introduction
-- ===============

-- We can accomplish this in Haskell '98 - but it's not fun!
-- Let's make things complicated by using some extensions!

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

------------------------------------------------------------------------------

-- There is one module.  We'll need a few elements from `System` for
-- handling the plumbing aspects, and a handful of elements from the
-- `containers` and `mtl` libraries. 

module Main where

import System.IO
import System.Process
import System.Environment

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Arrow
import Control.Concurrent

import qualified Data.List as L
import qualified Data.Set  as S

import Data.Either

------------------------------------------------------------------------------

-- We'll also be using Parsec for parsing, and JMacro for code generation

import qualified Text.Parsec.Token as T
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec hiding ((<|>), many)

import Language.Javascript.JMacro

------------------------------------------------------------------------------

-- So what does this language look like?

samplePrograms :: [String]
samplePrograms  = [

------------------------------------------------------------------------------

-- Simple values and infix operators will compile to their identical 
-- underlying implementations.  Yes, that means there are only `Num`s!

    "   2 + 2.0 == 4.0   ",                                         -- 0

-- * Infix operators directly from Javascript.
-- * Whole program evaluates to a single value.
-- * Primitive syntax similar to Javascript.

------------------------------------------------------------------------------

-- The language is typed, but there are no type annotations
-- (except in data constructors, more to follow ...).

    "   4 + \"whoops\"   ",                                         -- 1

-- * Strong statically typed, fully inferred.
-- * Whole program can be said to have a unique type - in this case `Num`.
-- * Language semantics guarantee a principal type.

------------------------------------------------------------------------------

-- You can introduce symbols with the `let` keyword.
-- (Note: `\` is Haskell's multiline syntax)

    "   let x = 1.0;     \
    \   x + 1.0 == 2.0   ",                                         -- 2

-- A `let` expression must introduce a body, so eg:

    "   let x = 1;   ",                                             -- 3

-- is syntactically illegal.

------------------------------------------------------------------------------

-- Functions - we'll refer to this as "Abstraction" to sound cool!
-- Also pictured: prefix application.

    "   (fun x -> x + 4) 2 == 6   ",                                -- 4

    "   let g = fun x -> x + 4;   \
    \   g 2 == 6                  ",                                -- 5

------------------------------------------------------------------------------

-- Pattern Matching (tm) FUN!

    "   let fib = fun n ->                     \
    \       match n with                       \
    \       0 -> 0;                            \
    \       1 -> 1;                            \
    \       n -> fib (n - 1) + fib (n - 2);;   \
    \   fib 7                                  ",                   -- 6

-- Also pictured - recursive reference `fib`.  Our pattern matching will
-- infer appropriate types as well.

    "   match 3 + 3 with \"gotcha\" -> false;   ",                  -- 7

------------------------------------------------------------------------------

-- User defined algebraic data type constructors.  The chosen syntax is
-- different from most MLs - but is quite easy to translate to the data
-- structure we will use to represent the inferred type of a sub expression.

    "   data Cons: a -> List a -> List a;        \
    \   data Nil: List a;                        \
    \                                            \
    \   let length = fun n ->                    \
    \       match n with                         \
    \           Nil -> 0;                        \
    \           (Cons _ xs) -> 1 + length xs;;   \
    \                                            \
    \   length (Cons 1 (Cons 2 Nil))             ",                 -- 8

-- This syntax does not permit GADTs - infering types in their presence is
-- out of scope.

------------------------------------------------------------------------------

-- Our type inference algorithm will be able to track any polymorphic types
-- introduced via `data`, and make sure they don't clash.

    "   data Just: a -> Maybe a;          \
    \   data None: Maybe a;               \
    \                                     \
    \   match Just 5 with                 \
    \       (Just \"blammo\") -> false;   ",                        -- 9

------------------------------------------------------------------------------

-- Our language is not going to have kind inference - so type variables must
-- be monotypes.

    "   data Just: a -> Maybe a;    \
    \   data None: Maybe a;         \
    \                               \
    \   data App: f x -> App f x;   \
    \                               \
    \   App (Just 10)               ",                             -- 10

-- .. nor will it allow GADTs

    "   data Gadt: String -> Gadt String;    \
    \   Gadt \"Zoink\"                       ",                    -- 11

------------------------------------------------------------------------------

-- ... and in general, it will do sensible things.

    "   let letly = 4;                    \
    \   (let func = fun x -> x + ((2));   \
    \   func letly)                       ",                       -- 12

    "   let x = 3;                \
    \   let f = fun y -> y + x;   \
    \   let x = 2;                \
    \   f 3 == 6                  " ]                              -- 13

------------------------------------------------------------------------------

-- 2. Compiler "Architecture"
-- ==========================

-- The structure of compilation can be expressed as a simple
-- function composition.  

compile :: String -> Either Err String
compile = parseOhml >=> typeCheck >=> generateJs >=> toText

newtype Err = Err String

------------------------------------------------------------------------------

-- The Abstract Syntax Tree, or AST in the 'biz.
-- We also say 'biz in da 'biz.

-- Here, `AbsExpr` is an Abstraction, and `AppExpr` is an Application.

data Expr = DatExpr TypeSym TypeAbs Expr   -- data True: Bool
          | LetExpr Sym Expr Expr          -- let x = 4; x
          | AppExpr Expr Expr              -- f x
          | AbsExpr Sym Expr               -- fun x -> x
          | VarExpr Val                    -- x
          | MatExpr Expr [(Patt, Expr)]    -- match 2 with x -> x

    deriving (Show)

-- (Note: We use GADT syntax solely for clarity)
    
------------------------------------------------------------------------------

-- Patterns are either `Val` or `Con` (which is really a decon, amirite?).
-- Note we do not distinguish between literal and symbol matching, 
-- because this is captured in the definition of `Val`

data Patt = ValPatt Val
          | ConPatt TypeSym [Patt]

    deriving (Show)

data Val = SymVal Sym
         | LitVal Lit
         | ConVal Type

    deriving (Show)

newtype Sym = Sym String deriving (Show)

data Lit = StrLit String
         | NumLit Double

    deriving (Show)

------------------------------------------------------------------------------

-- We would like to use the same data structure for both declared and infered
-- types, but they aren't quite the same ...

newtype TypeVar = TypeVarT String deriving (Show, Eq)
newtype TypeSym = TypeSymT String deriving (Show, Eq)
newtype TypeAbs = TypeAbsT Type deriving (Show, Eq)

data Type = TypeSym TypeSym        -- Int, String
          | TypeVar TypeVar        -- a, b
          | TypeApp Type Type      -- m a, b -> c, List Num

-- `TypeGen`s are a special `TypeVar` which we use to mark places where
-- we want a type to be polymorphic.  More to follow ...

          | TypeGen Int            -- forall a. a

    deriving (Show, Eq)

------------------------------------------------------------------------------

-- C. Parsing With Prejudice
-- =========================

-- ... goes to Hackage, downloads Parsec ...

-- `parseOhml` has a type very similar to Parsec's `Parser a` type already.

parseOhml :: String -> Either Err Expr
parseOhml = left (Err . show) . parse grammar "Parsing OHML"

------------------------------------------------------------------------------

-- `Parser a` is composable in a manner similar to what we saw in `compile`.
-- It is also an instance of the `Monad` type class, but we're not going
-- to use it; instead, we're going to use an arguably more interesting API,
-- the `Applicative` instance.

grammar :: Parser Expr
grammar = spaces *> exprP <* eof

-- Applicative functor combinators are left associative - as is
-- function application!

------------------------------------------------------------------------------

-- There is some static info we need to define about our language.  The 
-- keywords ...

keywords = [ "let", "fun", "match", "with", "data" ]

-- ... and operators, arranged in precedence order.

ops = [ [ "" ]
      , [ "^" ]
      , [ "*", "/" ]
      , [ "+", "-" ]
      , [ "<", "<=", ">=", ">", "==", "!=" ]
      , [ "&&", "||" ] ]

------------------------------------------------------------------------------

-- Parsec provides tokenization for free, given some basic rules about what
-- defines a keyword, operator, etc.

ohmlDef :: LanguageDef ()
ohmlDef = emptyDef {
    T.reservedNames   = keywords,
    T.reservedOpNames = L.concat ops,
    T.identStart      = lower <|> char '_'
}

-- Record wild card will bind locally all fields of the `TokenParser`, of
-- which there are many (5).

T.TokenParser { .. } = T.makeTokenParser ohmlDef

-- (5) http://legacy.cs.uu.nl/daan/download/parsec/parsec.html#TokenParser

------------------------------------------------------------------------------

-- The simplest parsers are for `Sym` and `TypeSym`.

symP :: Parser Sym
symP = Sym <$> identifier

typSymP :: Parser TypeSym
typSymP = (TypeSymT .) . (:) <$> upper <*> identifier

typVarP :: Parser Type
typVarP = TypeVar . TypeVarT <$> identifier <?> "Type Variable"

-- A neat product of the Parsec's combinator API is that the elemental
-- parsers are themselves full parsers, so they're easy to test.

test_symP = parse symP "" <$> [ "func", "lett", "f", "concat_map", "NoSym" ]
test_typSymP = parse typSymP  "" <$> [ "Boolean", "String", "noType"]


------------------------------------------------------------------------------

-- The Literal parser is a simple parser which generates Lit values.

litP :: Parser Lit
litP = stringL <|> numL

-- Here, `stringLiteral` and `naturalOrFloat` come from `T.TokenParser`.

    where
        stringL = StrLit <$> stringLiteral
        numL    = NumLit . toDouble <$> naturalOrFloat

        toDouble (Left i)  = fromInteger i
        toDouble (Right f) = f

test_litP = parse litP "" <$> [ "1", "3.14", "\"Hello\"" ]

------------------------------------------------------------------------------

-- "Alternative" combinator will evaluate to its second argument, iff
-- it's first argument fails and consumes no input - backtracking is explicit
-- via `try`.

valP :: Parser Val
valP =
    (SymVal <$> symP)
        <|> (LitVal <$> litP) 
        <|> ConVal . TypeSym <$> typSymP

test_valP = parse valP "" <$> [ "1.0", "\"test\"", "True", "x" ]

------------------------------------------------------------------------------

-- Pattern parser introduces `parens` and `many`

pattP :: Parser Patt
pattP =

    valPattP <|> conPattP <|> parens conPatsP

    where
        valPattP = ValPatt <$> valP
        conPattP = flip ConPatt [] <$> typSymP
        conPatsP = ConPatt <$> typSymP <*> many pattP <|> pattP

-- Patterns compose everything we've seen so far

test_pattP = parse pattP "" <$> [ "True", "(Cons x Nil)", "2", "x" ]

------------------------------------------------------------------------------

-- The `Expr` parser makes use of a number of `T.TokenParser` lexers.

exprP :: Parser Expr
exprP =

    letExprP
        <|> datExprP
        <|> absExprP 
        <|> matExprP 
        <|> appExprP 

-- and we obviously need to define all of these 

    where

------------------------------------------------------------------------------

-- TODO explain `reserved`

        absExprP =
            pure AbsExpr
                <*  reserved "fun"
                <*> symP
                <*  reservedOp "->"
                <*> exprP
                <?> "Abstraction"        

------------------------------------------------------------------------------

-- TODO

        matExprP =
            pure MatExpr
                <*  reserved "match"
                <*> exprP
                <*  reserved "with"
                <*> many1 (try caseP)
                <?> "Match Expression"

            where
                caseP =
                    (,) <$> pattP <*  reservedOp "->" <*> exprP <* semi

------------------------------------------------------------------------------

-- `LetExpr` and `DatExpr` are roughly identical; here they've been arranged
-- slightly differently to show the symmetry between function application
-- & combinators of the `Applicative` type class.

        letExprP =
            pure LetExpr  <*  reserved "let" 
                <*> symP  <*  reservedOp "="
                <*> exprP <*  semi
                <*> exprP <?> "Let Expression"

        datExprP =
            pure DatExpr    <*  reserved "data"
                <*> typSymP <*  reserved ":"
                <*> typAbsP <*  semi
                <*> exprP   <?> "Type Expression"

------------------------------------------------------------------------------

-- `buildExpressionParser` does the heavy lifting of 

        appExprP = buildExpressionParser opPs termP <?> "Application"

            where
                opPs =
                    [[ Infix (spaces >> return AppExpr) AssocLeft ]]
                        ++ toInfixTerm opConst AssocLeft (tail ops)

-- We use this function to construct the complex data structure used by
-- buildExpressionParser.

                toInfixTerm op assoc =
                    fmap . fmap $ 
                        flip Infix assoc
                        <<< uncurry (*>) 
                        <<< reservedOp
                        &&& return . op

                valExprP = VarExpr <$> valP <?> "Value"
                termP = valExprP <|> parens exprP
                opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym 

------------------------------------------------------------------------------

-- Putting it all together in one parser ...

typAbsP :: Parser TypeAbs
typAbsP = TypeAbsT <$> typP <?> "Type (TypeAbs)"

typP :: Parser Type
typP = buildExpressionParser opPs termP <?> "Type Symbol"

    where
        opPs =
            [ [Infix (spaces >> return TypeApp) AssocLeft]
            , [Infix (reservedOp "->" >> return (fnConst "->")) AssocRight] ]

        fnConst = (TypeApp .) . TypeApp . TypeSym . TypeSymT
        termP   = typVarP <|> TypeSym <$> typSymP

------------------------------------------------------------------------------

-- ... and that's it!

test_parseExpr = parseOhml <$> samplePrograms

------------------------------------------------------------------------------

-- CHAPTER FOUR: Type Inference
-- ============================

-- Type checking is a broad subject - this is a very simple implementation
-- which neverthless allows for much of the expressiveness you may be
-- famliar with in Haskell.  The strategy basically looks like this:

--    * Introduce an `Assumption` for every symbol in a program,
--      binding the symbol's name & scope to it's type, which may be a
--      variable.

--    * Walk the expression tree using these assumptions, and introduce a
--      `Substitution` for every usage of a symbol, to fit its context.

--    * Where we assign or abstract values, we need to `Unify` two types,
--      by calculating the most general new substitutions which make them
--      equivalent.

--    * Where we want to use a variable in a polymorphic way, we'll need 
--      to `Generalize` some assumptions so they can be instantiated with
--      a fresh set of type variables for every usage. 

------------------------------------------------------------------------------

-- We're going to borrow the approach taken in "Typing Haskell in 
-- Haskell" (1).  The type of a program will be computed via
-- the `StateT` monad transformer, which will hide our substitution
-- environment and unique type variable generator.

type TypeCheck a = StateT (Subst, Int) (Either Err) a

typeCheck :: Expr -> Either Err Expr
typeCheck =
    uncurry fmap 
         <<< const
         &&& flip runStateT defState
         . exprCheck prelude

defState = ([], 0)

-- (1) http://web.cecs.pdx.edu/~mpj/thih/

------------------------------------------------------------------------------

-- ... and introduce our first computation in this monad, a new type variable
-- factory.

newTypeVar :: TypeCheck Type
newTypeVar = do
    (s, i) <- get
    put (s, i + 1)
    return (TypeVar (TypeVarT ("tvar_" ++ show i)))

-- These will help us keep large types concise.

toTyp = TypeSym . TypeSymT

infixr 4 `fn`
fn :: Type -> Type -> Type
fn = TypeApp . TypeApp (toTyp "->")

------------------------------------------------------------------------------

-- Assumptions
-- -----------

-- Assumptions are used to keep track of the type of symbols, but only at
-- the time they are added to the `[Ass]`.  We'll need to apply the
-- current `Subst` to know the real type of a symbol given the total 
-- information known so far.

data Ass = String :>: TypeAbs

-- There are some symbols which are implicit on our language, and they look
-- like this:

prelude :: [Ass]
prelude =

    [ "==" :>: TypeAbsT (TypeGen 0 `fn` TypeGen 0 `fn` toTyp "Bool")
    , "&&" :>: TypeAbsT (toTyp "Bool" `fn` toTyp "Bool" `fn` toTyp "Bool")
    , "+"  :>: TypeAbsT (toTyp "Num" `fn` toTyp "Num" `fn` toTyp "Num")
    , "-"  :>: TypeAbsT (toTyp "Num" `fn` toTyp "Num" `fn` toTyp "Num") ]

------------------------------------------------------------------------------

-- When we encounter a symbol, we just look it up in the list of assumptions.
-- This is a convenient opportunity to check for undefined symbols!

find :: String -> [Ass] -> TypeCheck TypeAbs
find i [] = typErr ("Unbound identifier: " ++ i)
find i ((i' :>: sc) : as) 
    | i == i'   = return sc
    | otherwise = find i as

------------------------------------------------------------------------------

-- Substitutions
-- -------------

type Subst = [(TypeVar, Type)]

-- There are some basic rules to extending substitutions.

ext :: Subst -> Subst -> Subst
ext new old = [ (u, apply new t) | (u,t) <- old ] ++ new

extSubst :: Subst -> TypeCheck ()
extSubst new = get >>= return . first (ext new) >>= put

------------------------------------------------------------------------------

-- Substitutions can be applied to types, and it will be useful for 
-- calculating substitutions to have a way to get the free `TypeVar`s
-- in a `Type`.

class Substitute a where
    apply :: Subst -> a -> a
    getVars :: a -> [TypeVar]

------------------------------------------------------------------------------

instance Substitute Type where
    apply s (TypeVar (flip lookup s -> Just u)) = u
    apply _ (TypeVar u) = TypeVar u
    apply s (TypeApp l r) = TypeApp (apply s l) (apply s r)
    apply _ t = t
    
    getVars (TypeVar u) = [u]
    getVars (TypeApp l r) = getVars l `L.union` getVars r
    getVars _ = []

instance Substitute a => Substitute [a] where
    apply s = map (apply s)
    getVars = L.nub . concat . map getVars

instance Substitute Ass where
    apply s (i :>: sc) = i :>: (apply s sc)
    getVars (_ :>: sc) = getVars sc

instance Substitute TypeAbs where
    apply s (TypeAbsT qt) = TypeAbsT (apply s qt)
    getVars (TypeAbsT qt) = getVars qt

------------------------------------------------------------------------------

-- Unification
-- -----------

-- Unification should modify the substitution environment such that
-- `apply s t == apply s u`.  First apply the current substitution to each,
-- then calculate the Most General Unifier

unify :: Type -> Type -> TypeCheck ()
unify t u = do 
    s <- fst <$> get
    apply s t `mgu` apply s u

------------------------------------------------------------------------------

-- To calculate the most general unifier, recursively descend their 
-- structures until we come to identical types, or a type variable.

mgu :: Type -> Type -> TypeCheck ()
mgu (TypeApp f x) (TypeApp g y) = unify f g >> unify x y
mgu (TypeSym t) (TypeSym u) | t == u = return ()
mgu (TypeVar u) t = u `varBind` t
mgu t (TypeVar u) = u `varBind` t
mgu t u = uniErr "Types do not unify" t u

test_mgu = runStateT (toTyp "String" `mgu` toTyp "Num") defState

------------------------------------------------------------------------------

-- Once we've committed to creating a new substitution, we need to make
-- sure it's valid by checking that we are not constructing a replacement
-- for a type into itself - an 'occurs' check.

varBind :: TypeVar -> Type -> TypeCheck ()
varBind u t 
    | t == TypeVar u     = return ()
    | u `elem` getVars t = uniErr "Occurs check failed" u t
    | otherwise          = extSubst [(u, t)]            
           
test_occurs = ohml

    "   data Cons: a -> List a -> List a;     \
    \   data Nil: List a;                     \
    \   let f = fun x ->  match x with        \
    \       (Cons _ xs) -> Cons (f xs) xs;;   \
    \   f Nil                                 "

------------------------------------------------------------------------------

-- Generalization
-- --------------

-- Consider the program

test_generalization = ohml 

    "   let f = fun x -> x;               \
    \   f 1 == f 1                        \
    \       && f \"test\" == f \"test\"   "

-- Without generalization step, the application of `f 1` will introduce the
-- assumption `f : Num -> Num`, which makes the rest of the expression
-- invalid.

-- Generalization will guarantee that each application of `f` will have
-- new type variables, by replacing the type variable representing `x` with
-- a fresh var at every invocation.

------------------------------------------------------------------------------

-- Simple generalization is simply the process of replacing some type
-- variables with `TypeGen`, whose integer argument represents the index
-- in the `TypeAbs`s argument list.

quantify :: [TypeVar] -> Type -> TypeAbs
quantify vars typ = TypeAbsT (apply subs typ)
    where
        qVars = filter (flip elem vars) (getVars typ)
        subs  = zip qVars (map TypeGen [ 0 .. ])

------------------------------------------------------------------------------

-- With these tools, we can construct an environment aware `generalize`, which
-- applies the current substitution and only generalizes the free `TypeVar`s
-- in `valT`.

generalize :: [Ass] -> Type -> TypeCheck TypeAbs
generalize as valT = do

    subs <- fst <$> get 
    return (quantify (getFree subs) (apply subs valT))

    where
        getFree subs = getS subs valT L.\\ getS subs as
        getS x = (getVars .) . apply $ x

------------------------------------------------------------------------------

-- ... and instantiating them again.

freshInst :: TypeAbs -> TypeCheck Type
freshInst (TypeAbsT qt) = do
    ts <- sequence (take (getGen 0 qt) (repeat newTypeVar))
    return (inst ts qt)

    where
        inst ts (TypeApp l r) = TypeApp (inst ts l) (inst ts r)
        inst ts (TypeGen n) = ts !! n
        inst _ t = t

        getGen n (TypeApp l r) = max (getGen n l) (getGen n r)
        getGen n (TypeGen r) = max n (r + 1)
        getGen n t = n

------------------------------------------------------------------------------

-- Inference Itself
-- ----------------

-- Literals are trivial and require none of the machinery we just spent
-- 10 slides explaining.  Stay with me for a few minutes though ...

litCheck :: Lit -> Type
litCheck (StrLit _) = toTyp "String"
litCheck (NumLit _) = toTyp "Num"

------------------------------------------------------------------------------

-- Patterns are a bit more complicated.  They can introduce symbols, so
-- we must return a list of these `Ass`s as well as the their `Type a`.

pattCheck :: [Ass] -> Patt -> TypeCheck ([Ass], Type)

pattCheck _ (ValPatt (LitVal l)) =
    return ([], litCheck l)

pattCheck _ (ValPatt (SymVal (Sym s))) = do
    t <- newTypeVar
    return ([ s :>: TypeAbsT t ], t)

------------------------------------------------------------------------------

-- Simple constructor patterns can be checked by introducing a fresh 
-- instance of their `TypeAbs`.  

pattCheck as (ValPatt (ConVal (TypeSym (TypeSymT l)))) = do
    sc <- find l as
    t  <- freshInst sc
    return ([], t)

------------------------------------------------------------------------------

-- In order to check destruction patterns, we need to recreate the implied
-- (abstraction) type of the arguments, and unify with the constructor's
-- `Ass` from the environment.  

pattCheck as (ConPatt (TypeSymT con) ps) = do
    sc <- find con as
    x  <- mapM (pattCheck as) ps
    t' <- newTypeVar
    t  <- freshInst sc
    unify t (foldr fn t' (map snd x))
    return (L.concat (map fst x), t')

------------------------------------------------------------------------------

-- Expression checking is the most complex.  Literals are lifted trivially.

exprCheck :: [Ass] -> Expr -> TypeCheck Type

exprCheck _ (VarExpr (LitVal l)) =
    return (litCheck l)

------------------------------------------------------------------------------

exprCheck as (VarExpr (SymVal (Sym sym))) =
    find sym as >>= freshInst

exprCheck as (VarExpr (ConVal (TypeSym (TypeSymT sym)))) =
    find sym as >>= freshInst

-- If you read a theoretical treatment of HM, you will encounter equations
-- that look like this.  The cases of `exprCheck` map roughly to these
-- lifted from wikipedia:

--  x : σ ∈ Γ
--  ----------
--  Γ ⊦ x : σ

-- where
--  σ    = a type scheme, `TypeAbs`
--  τ    = a type, `Type`
--  Γ    = a type environment, `TypeCheck a`
--  ⊦     = an assertion.
--  :    = an assumption,  `Ass` type
-- ---   = a judgment, premise is the numerator, conclusion is the denominator

------------------------------------------------------------------------------

-- This is the `let` generalization alluded to previously.

exprCheck as (LetExpr (Sym sym) val expr) = do
    symT <- newTypeVar
    valT <- exprCheck (sym :>: TypeAbsT symT: as) val
    unify valT symT
    schT <- generalize as valT 
    exprCheck (sym :>: schT : as) expr

-- This generalization is implied by the premise `eₒ : σ`

--  Γ ⊦ eₒ : σ   Γ, x : σ ⊦ e₁ : τ
--  ------------------------------
--  Γ ⊦ let x = eₒ in e₁ : τ

------------------------------------------------------------------------------

-- Application checking simply involves verifying the parameter types unify.

exprCheck as (AppExpr f x) = do
    fT   <- exprCheck as f
    xT   <- exprCheck as x
    appT <- newTypeVar
    unify (xT `fn` appT) fT
    return appT

--  Γ ⊦ eₒ : τ → τ'   Γ ⊦ e₁ : τ
--  ----------------------------
--  Γ ⊦ eₒ e₁ : τ'

------------------------------------------------------------------------------

-- Abstraction in our language is easy to typecheck, as we will not
-- generalize the parameter x (notice that `x : τ`)

exprCheck as (AbsExpr (Sym sym) expr) = do
    symT <- newTypeVar
    res  <- exprCheck (sym :>: TypeAbsT symT : as) expr
    return (symT `fn` res)

--  Γ, x : τ ⊦ e : τ'
--  --------------------
--  Γ ⊦ λ x . e : τ → τ' 

------------------------------------------------------------------------------

-- Pattern matching is just a special case of abstraction & application.
-- We accomplish this recursively to make sure each case is the same.

exprCheck as (MatExpr expr patts) = do
    exprT <- exprCheck as expr
    argCheck exprT patts

    where
        argCheck exprT ((patt, res):es) = do
            (pattAs, pattT) <- pattCheck as patt
            unify exprT pattT
            argRecCheck exprT pattAs res es

        argRecCheck _ pattAs res [] =
            exprCheck (pattAs ++ as) res

        argRecCheck exprT pattAs res es = do
            resT  <- exprCheck (pattAs ++ as) res
            esT   <- argCheck exprT es
            unify resT esT
            return resT

------------------------------------------------------------------------------

-- `DatExpr a` simply requires that we introduce a new assumption for this
-- constructor.  We borrow part of our generalization mechanism to make
-- these constructors fully polymorphic for all free type variables.

exprCheck as (DatExpr (TypeSymT name) (TypeAbsT typ) expr) = do
    checkValid typ
    checkMono typ
    exprCheck (name :>: typKAbs : as) expr

    where
        typK = typ
        typKAbs = quantify (getVars typK) typK

------------------------------------------------------------------------------

-- Before we can introduce an assumption for a data constructor, we need to
-- make sure it is valid.  `checkValid` enforces that the constructor's 
-- return value is of the form `Type var var ...`

checkValid :: Type -> TypeCheck ()
checkValid (isFun -> Just (_, k)) = checkValid k
checkValid (TypeApp k (TypeVar _)) = checkValid k
checkValid (TypeApp k n) = typErr ("Invalid constructor: " ++ show n)
checkValid (TypeVar n) = typErr ("Invalid constructor: " ++ show n)
checkValid (TypeSym _) = return ()

------------------------------------------------------------------------------

-- `checkMono` makes sure type variables are monotypes - unlike Haskell,
-- we won't be allowing types like `f String`

checkMono :: Type -> TypeCheck()
checkMono = recMono True

    where
        recMono _ (TypeApp f x) =
            recMono False f >> recMono True x
        recMono False (TypeVar n) =
            typErr ("Polytype type variable: " ++ show n)
        recMono _ _ =
            return ()

------------------------------------------------------------------------------

test_check = lefts (testCheck <$> samplePrograms)

------------------------------------------------------------------------------

-- e.) Code Generation
-- ===================

-- Q: What is JMacro?

-- A: JMacro is a library for the programmatic generation of Javascript code.
--    It is designed to be multipurpose -- it is useful whether you are 
--    writing nearly vanilla Javascript or you are programmatically generating
--    Javascript either in an ad-hoc fashion or as the backend to a compiler
--    or EDSL. (1)

-- Sounds useful, if only we were in the midst of writing a Javascript
-- compiler backend ...

-- (1) http://www.haskell.org/haskellwiki/Jmacro

------------------------------------------------------------------------------

-- Features we can use without trying:

-- * Javascript AST and renderer.

--       TODO example

-- * Quasiquoted interface which also provides some new syntax, like
--   destructive bindings, optional haskell style syntax sugar.

-- * Hygienic names - this let's us declare new vars without 
--   knowledge of the environment.

------------------------------------------------------------------------------

-- There's alot going on here that's new: 


generateJs = Right . consoleLog . toJExpr  -- (1)

    where
        consoleLog x = [jmacroE|           // (2)
            function() {                   // (3)
                var y = `(x)`;             // (4)
                console.log(y);
            }()
        |]                                 -- (5) 

-- (1) toJExpr, and the `ToJExpr a` class
-- (2) quasiquotation - wait what just happened?
-- (3) HOLY **** WE'RE INSIDE JAVASCRIPT!
-- (4) HOLY **** WE JUST REFERENCED A HASKELL VALUE FROM INSIDE JAVASCRIPT!
-- (5) Ok, that was weird ...

------------------------------------------------------------------------------

-- Marshalling the OHML AST into `JExpr`s - the strategy is to rely
-- on instances of `ToJExpr` for our AST.

instance ToJExpr Lit where
    toJExpr (StrLit s)  = toJExpr s
    toJExpr (NumLit n)  = toJExpr n

instance ToJExpr Sym where
    toJExpr (Sym x) = ref x

instance ToJExpr Val where
    toJExpr (SymVal s) = toJExpr s
    toJExpr (LitVal l) = toJExpr l
    toJExpr (ConVal (TypeSym (TypeSymT s))) = ref s

------------------------------------------------------------------------------

-- ... but the quasiquoter interface does not allow dynamic variable names,
-- so for this and some other tasks we must construct the JExpr manually.

ref :: String -> JExpr
ref = ValExpr . JVar . StrI

test_jexpr = [jmacroE| `(ref "test")` |]

------------------------------------------------------------------------------

-- This enables us also to do one of the only manual processes necessary -
-- at some point we'll need to introduce new variables into scope,
-- with a specific name and value.

intro :: (ToJExpr a) => String -> (JExpr -> a) -> Expr -> JExpr
intro sym f expr = [jmacroE| 
    function(arg) {                              // (1)
        `(DeclStat (StrI sym) Nothing)`;         // (2)
        `(ref sym)` = `(f arg)`;                 // (3)
        return `(expr)`;
    }
|]

-- (2) equivalent to `var x;`

-- (1) `arg` is created in javascript ...
-- (3) ... but we can use it as an argument from Haskell?

-- GO HOME HASKELL, YOU'RE DRUNK

------------------------------------------------------------------------------

-- Converting our `Expr` type to JMacro's `JExpr` is simple for Vars, 
-- Abstractions and applications.

instance ToJExpr Expr where

    toJExpr (VarExpr v) =
        toJExpr v

    toJExpr (AbsExpr (Sym sym) ex) = 
        intro sym id ex

-- Infix operators will need to be constructed manually

    toJExpr (isInfix -> Just (x, o, y)) =
        InfixExpr o (toJExpr x) (toJExpr y)

    toJExpr (AppExpr f x) = 
        [jmacroE| `(f)`(`(x)`) |]

------------------------------------------------------------------------------

-- A let can be reduced to an application of an abstraction (though only 
-- in the backend - during type checking, we had generalization to contend
-- with).

    toJExpr (LetExpr (Sym sym) ex expr) = [jmacroE| 

        `(intro sym (const ex) expr)`()

    |]

-- `TypeExpr`s require are the hardest so far, requiring us to construct
-- a valid, curried constructor function for.  More to follow ...

    toJExpr (DatExpr (TypeSymT sym) typsch expr) = [jmacroE|

        function() {
            var scheme = `(curriedFun sym typsch)`;
            return `(intro sym (const scheme) expr)`()
        }()

    |]

------------------------------------------------------------------------------

-- The difficulties involved in matching are punted to the `ToJExpr Match` 
-- which will be defined later on, here we simple check that a match is
-- `true`.  Note the use of `scope` as a parameter to the datatype constructor
-- `Match`, allowing us to interpolate hygienic variables.

    toJExpr (MatExpr val ((patt, expr):cases)) = [jmacroE|

        (function() {
            var scope = this;
            var vall = `(val)`;
            if (`(Match vall patt scope)`)
                return `(expr)`
            else
                return `(MatExpr val cases)`;
        })()

    |]

-- And the result of a strictness failure ...

    toJExpr (MatExpr _ []) = [jmacroE|
        (function() { throw "Pattern Match Exhausted"; })()
    |]

------------------------------------------------------------------------------

-- We saw two new constructs during `Expr` rendering.  `curriedFun` is to
-- artificially construct a curried version of a data constructors.  We're
-- going to use the first call in the curried chain to introduce an array to
-- capture arguments through the partial application.  This means we need to
-- treat constructor functions and empty constructors differently.

curriedFun :: String -> TypeAbs -> JExpr

curriedFun sym (TypeAbsT (isFun -> Just (_, fs))) = [jmacroE|

    function(x) {
        var args = [];
        `(args)`.push(x); 
        return `(curriedFun' sym args (TypeAbsT fs))`;
    }

|]

curriedFun sym ts = curriedFun' sym "" ts

------------------------------------------------------------------------------

-- .. and then recurse as expected, passing the `args` accumulator down ...

curriedFun' sym args (TypeAbsT (isFun -> Just (_, fs))) = [jmacroE|

    function(x) {
        `(args)`.push(x); 
        return `(curriedFun' sym args (TypeAbsT fs))`;
    }

|]

-- ... and in the base case, return an instance of 

curriedFun' sym args _ = [jmacroE| 

    {
        attrs: `(args)`,
        type: `(sym)`
    }

|]

------------------------------------------------------------------------------

-- We also saw `Match` earlier, a datatype to represent the javascript
-- generation of the application of a value to a pattern - as this is 
-- used in the cond position of an `if` statement earlier.

data Match = Match JExpr Patt JExpr deriving (Show)

instance ToJExpr Match where

-- Literal matches are a simple equality test.

    toJExpr (Match val (ValPatt (LitVal l)) _) =

        [jmacroE| `(l)` == `(val)` |]

------------------------------------------------------------------------------

-- Symbol matches introduce a new property onto the `scope` object

    toJExpr (Match val (ValPatt (SymVal (Sym s))) scope) = [jmacroE|

        (function() {
            `(scope)`[`(s)`] = `(val)`;
            return true;
        })()

    |]

-- Datatype constructors without arguments are checked via their `type`
-- property.

    toJExpr (Match val (ValPatt (ConVal (TypeSym (TypeSymT s)))) scope) =

        [jmacroE| `(val)`.type == `(s)` |]

------------------------------------------------------------------------------

-- A `ConPatt` with arguments, however, needs to recursively unwrap its
-- arguments as `Match`s.

    toJExpr (Match val (ConPatt (TypeSymT sym) ps) scope) = [jmacroE|

        `(val)`.type == `(sym)` && (function() {
            var result = true;
            for (var arg in `(val)`.attrs) {
                var argg = `(val)`.attrs[arg];
                result = result && `(conds argg ps scope)`;
            }
            return result;
        })()

    |]

------------------------------------------------------------------------------

conds :: JExpr -> [Patt] -> JExpr -> JExpr

conds _ [] _ = [jmacroE| true |]
conds val (x : xs) scope = [jmacroE|

    `(Match val x scope)` && `(conds val xs scope)`

|]

------------------------------------------------------------------------------

-- We're done!  Run all the sample programs to prove it!

test_all = sequence (ohml <$> samplePrograms)

-- THE END

------------------------------------------------------------------------------

-- UTILS

------------------------------------------------------------------------------

-- Main

toText :: JExpr  -> Either Err String
toText = Right . show . renderJs  

unwrap :: (Show a, Show b) => Either a b -> IO ()
unwrap (Right x) = putStrLn (show x) >> putStrLn "----------------------"
unwrap (Left x)  = putStrLn (show x) >> putStrLn "----------------------"

main :: IO ()
main = do

    file   <- head <$> getArgs
    source <- readFile file
    case compile source of
        Left  e  -> print e
        Right js -> putStrLn js

------------------------------------------------------------------------------

-- Views

isInfix :: Expr -> Maybe (Expr, String, Expr)
isInfix (AppExpr (AppExpr (VarExpr (SymVal (Sym o))) x) y) 
    | o `elem` concat ops  = Just (x, o, y)
isInfix _ = Nothing

isFun :: Type -> Maybe (Type, Type)
isFun (TypeApp (TypeApp (TypeSym (TypeSymT "->")) x) y) = Just (x, y)
isFun _ = Nothing

------------------------------------------------------------------------------

-- Type errors

typErr :: String -> TypeCheck a
typErr = lift . Left . Err

uniErr :: (Show t, Show u) => 
          String -> t -> u -> TypeCheck a

uniErr msg t u = typErr $
    msg ++ "\n  "
        ++ show u ++ " and " 
        ++ show t

instance Show Err where

    show (Err x) = "ERROR " ++ x

------------------------------------------------------------------------------

-- Node Tests

testCheck = parseOhml >=> typeCheck
testJMacro = parseOhml >=> typeCheck >=> generateJs

node :: String -> IO ()
node js = do

    (Just std_in, Just std_out, _, p) <-
        createProcess (proc "node" []) { std_in = CreatePipe, std_out = CreatePipe }

    forkIO $ do 
        errors <- hGetContents std_out
        putStr errors
        hFlush stdout         

    hPutStrLn std_in $ js ++ "\n\n"

    z <- waitForProcess p
    
    return ()

------------------------------------------------------------------------------


ohml :: String -> IO ()
ohml x = do
    putStr "Program:  "
    putStrLn $ concat $ L.intersperse " " $ words x
    putStr "Node.js:  "
    case compile x of
        Right x -> node x
        Left y  -> putStrLn (show y)
    putStrLn ""

------------------------------------------------------------------------------

-- Slide maker

format :: IO ()
format = do
    text <- readFile "src/Ohml.hs"
    let slides = concat $ map replace $ zip [1 ..] $ map pad $ L.groupBy isSlide (lines text)
    putStrLn $ show $ (length $ L.groupBy isSlide (lines text)) - 9
    writeFile "slides.hs" (unlines slides)

------------------------------------------------------------------------------

    where 
        isSlide _ "------------------------------------------------------------------------------" = False
        isSlide _ _ = True

        replace (i, (_:xs)) = (take 72 (repeat ' ') ++ "-- " ++ show i) : map unline xs

        unline "------------------------------------------------------------------------------" = ""
        unline x = x

        pad xs | length xs < 28 =
            if (28 - length xs) `mod` 2 == 0
            then let sep = take ((28 - length xs) `quot` 2) (repeat "") in sep ++ xs ++ sep
            else let sep = take ((28 - length xs) `quot` 2) (repeat "") in (sep ++ [""]) ++ xs ++ sep
        pad xs = error $ unlines xs

------------------------------------------------------------------------------


-- LICENSE
-- =======

-- Copyright (c) 2013 Andrew Stein

-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTypeCheckES OF MERCHANTABILITY, FITNESS FOR A PARTypeCheckCULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTypeCheckON OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTypeCheckON WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

