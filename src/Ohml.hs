-------------------------------------------------------------------------------

-- One Hour ML, or How I Learned To Stop Worrying And Write An 
-- ML To Javascript Compiler In About One Hour.

-------------------------------------------------------------------------------

-- (1) [Write Yourself a Scheme in 48hrs]
--     (http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours),

-- (2) [Parsec]
--     (http://www.haskell.org/haskellwiki/Parsec)

-- (3) [JMacro]
--     (http://www.haskell.org/haskellwiki/Jmacro)

-- (4) [Typing Haskell in Haskell]
--     (http://web.cecs.pdx.edu/~mpj/thih/)

-------------------------------------------------------------------------------

--                     Learn Forml, or I'll claw up your furniture!
--              /\   / 
--          )  ( ')  
--         (  /  )
--          \(__)|

-- I. -------------------------------------------------------------------------

-- Why?

-- * Fun!
-- * ...

-------------------------------------------------------------------------------

-- We can accomplish this in Haskell '98 - but it's not fun!
-- Let's make things complicated by using some extensions!

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------

-- There is one module.  We'll need the environment to take input, and a
-- handful of elements from the `containers` and `mtl` libraris. 

module Main where

import System.Environment

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Arrow

import qualified Data.List as L

-------------------------------------------------------------------------------

-- We'll also be using Parsec for parsing, and JMacro for code generation

import qualified Text.Parsec.Token as T
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec hiding ((<|>), many)

import Language.Javascript.JMacro

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
    \   fib 7                                  ",

-------------------------------------------------------------------------------

-- User defined algebraic data type constructors.
-- (TODO Address GADTs - they are allowed syntactically, but our inference
-- algorithm will not check them correctly)

    "   data Cons: forall a => a -> List a -> List a;   \
    \   data Nil: forall a => List a;                   \
    \                                                   \
    \   let length = fun n ->                           \
    \       match n with                                \
    \           Nil -> 0;                               \
    \           (Cons _ xs) -> 1 + length xs;;          \
    \                                                   \
    \   length (Cons 1 (Cons 2 Nil))                    " ]

-------------------------------------------------------------------------------

-- And of course, our language will not do dumb things while parsing

brokenSamples :: [String]
brokenSamples = [    

    "   let letly = 4; (let func = fun x -> x + ((2)); func letly)   ",

-------------------------------------------------------------------------------

-- ... or while type checking

    "   4 + \"whoops\"   ",

    "   match 3 + 3 with \"gotcha\" -> false;   ",

    "   data Just: forall a => a -> Maybe a;  \
    \   data None: forall a => Maybe a;       \
    \                                         \
    \   match Just 5 with                     \
    \       (Just \"blammo\") -> false;       ",

-------------------------------------------------------------------------------

-- ... or while generating javascript code.

    "   let x = 3;                \
    \   let f = fun y -> y + x;   \
    \   let x = 2;                \
    \   f 3 == 6                  " ]

-- I.C. -----------------------------------------------------------------------

-- Compiler Architecture

-------------------------------------------------------------------------------

-- The structure of compilation can be expressed as a simple
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

data Expr where

    LetExpr :: Bind -> Expr -> Expr           -- let x = 4; x
    AppExpr :: Expr -> Expr -> Expr           -- f x
    AbsExpr :: Sym  -> Expr -> Expr           -- fun x -> x
    VarExpr :: Val  -> Expr                   -- x
    MatExpr :: Expr -> [(Patt, Expr)] -> Expr

    deriving (Show)

-- (Note: We use GADT syntax solely for clarity - even though
-- this is not necessary)

-------------------------------------------------------------------------------

data Bind where

    SymBind :: Sym -> Expr -> Bind
    TypBind :: TypeSym () -> TypeSch () -> Bind

    deriving (Show)
    
-------------------------------------------------------------------------------

-- Patterns are either `Val` or `Con` (which is really a decon, amirite?).
-- Note we do not distinguish between literal and symbol matching, 
-- because this is captured in the definition of `Val`

data Patt where

    ValPatt :: Val -> Patt
    ConPatt :: TypeSym () -> [Patt] -> Patt

    deriving (Show)

-------------------------------------------------------------------------------

-- ... which looks like this.

data Val where

    SymVal  :: Sym -> Val
    LitVal  :: Lit -> Val
    ConVal  :: Type () -> Val

    deriving (Show)

-------------------------------------------------------------------------------

-- Symbols and literals, yada yada yada.

newtype Sym = Sym String deriving (Show, Eq)

data Lit where

    StrLit  :: String -> Lit
    NumLit  :: Double -> Lit

    deriving (Show)

-------------------------------------------------------------------------------

-- TODO a few demonstation expressions w/ translations

-------------------------------------------------------------------------------

-- Oh, and one more thing ...

-------------------------------------------------------------------------------

-- We would like to use the same data structure for both declared and infered
-- types, but we also want to present the talk incrementally - so

data TypeVar a where

    TypeVarP :: String -> TypeVar ()
    TypeVarT :: Kind -> String -> TypeVar Kind

data TypeSym a where 

    TypeSymP :: String -> TypeSym ()
    TypeSymT :: Kind -> String -> TypeSym Kind

data TypeSch a where

    TypeSchP :: [TypeVar ()] -> Type () -> TypeSch ()
    TypeSchT :: [Kind] -> Type Kind -> TypeSch Kind


-- TODO finish this section please!

data Type a where

    TypeSym :: TypeSym a -> Type a         -- Int, String
    TypeVar :: TypeVar a -> Type a         -- a, b
    TypeApp :: Type a -> Type a -> Type a  -- m a, b -> c

    TypeGen :: Int -> Type Kind            -- forall a. a

-- III. -----------------------------------------------------------------------

-- Parsing With Prejudice 

-------------------------------------------------------------------------------

-- ... goes to Hackage, downloads Parsec ...

-------------------------------------------------------------------------------

-- What is a Parser?  Well, this is a pretty good definition, which allows us
-- to define some combinators that also produce `MyParser`

type MyParser a = String -> Either Err (a, String)

-------------------------------------------------------------------------------

-- TODO Explain `Parser` and `GenParser`

-------------------------------------------------------------------------------

-- TODO Applicative Functor, Functor & Monad instances.

-------------------------------------------------------------------------------

-- With this in mind, we can define the parser simply with the `parse`
-- from Parsec.

parseOhml :: String -> Either Err Expr
parseOhml = left (Err . show) . parse grammar "Parsing OHML" 

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
    T.reservedOpNames = L.concat ops,
    T.identStart      = lower <|> char '_'
}

-- Record wild card will bind locally all fields of the `TokenParser`, of
-- which there are many (5).

T.TokenParser { .. } = T.makeTokenParser ohmlDef

-- (5) http://legacy.cs.uu.nl/daan/download/parsec/parsec.html#TokenParser

-------------------------------------------------------------------------------

grammar :: Parser Expr
grammar = spaces *> exprP <* eof

--       (spaces *> exprP) <* eof

-- Applicative functor combinators are left associative - so is
-- function application!

-------------------------------------------------------------------------------

-- The simplest parsers are for `Sym` and `TypeSym`

symP :: Parser Sym
symP = Sym <$> identifier

typSymP :: Parser (TypeSym ())
typSymP = (TypeSymP .) . (:) <$> upper <*> identifier

-- "Alternative" combinator will evaluate to its second argument, iff
-- it's first argument fails and consumes no input - backtracking is explicit
-- via `try`.

valP :: Parser Val
valP =
    (SymVal <$> symP)
        <|> (LitVal <$> litP) 
        <|> ConVal . TypeSym <$> typSymP

-------------------------------------------------------------------------------

-- The Literal parser is a simple parser which generates Lit values.

litP :: Parser Lit
litP = stringL <|> numL

-- Here, `stringLiteral` and `naturalOrFloat` come from `T.TokenParser`.

    where
        stringL = StrLit <$> stringLiteral
        numL    = NumLit . toDouble <$> naturalOrFloat

        toDouble (Left i)  = fromInteger i
        toDouble (Right f) = f

-------------------------------------------------------------------------------

-- Pattern parser introduces `parens` and `many`

pattP :: Parser Patt
pattP =

    valPattP <|> conPattP <|> parens conPatsP

    where
        valPattP = ValPatt <$> valP
        conPattP = flip ConPatt [] <$> typSymP
        conPatsP = ConPatt <$> typSymP <*> many pattP <|> pattP

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
            pure AbsExpr
                <*  reserved "fun"
                <*> symP
                <*  reservedOp "->"
                <*> exprP
                <?> "Abstraction"        

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

        letExprP =
            pure ((LetExpr .) . SymBind)
                <*  reserved "let" 
                <*> symP
                <*  reservedOp "="
                <*> exprP
                <*  semi
                <*> exprP
                <?> "Let Expression"

        typExprP =
            pure ((LetExpr .) . TypBind)
                <*  reserved "data"
                <*> typSymP
                <*  reserved ":"
                <*> typSchP
                <*  semi
                <*> exprP
                <?> "Type Kind Expression"

-------------------------------------------------------------------------------

-- TODO explain `buildExpressionParser`, allude to `ifx`

        appExprP =

            genExprP opConst AssocLeft ops termP <?> "Application"

            where
                valExprP =
                    VarExpr <$> valP <?> "Value"
                termP =
                    (valExprP <|> parens exprP) `chainl1` return AppExpr
                opConst =
                    (AppExpr .) . AppExpr . VarExpr . SymVal . Sym 

-------------------------------------------------------------------------------

-- TODO explain this crap

ifx op assoc =

    map $ map
        $ flip Infix assoc 
        . uncurry (*>) 
        . (reservedOp &&& return . op)

genExprP =

    ((buildExpressionParser .) .) . ifx

-------------------------------------------------------------------------------

typP :: Parser (Type ())
typP = typAppP <?> "Type Symbol"

    where
        typAppP = genExprP fnConst AssocRight [[ "->" ]] termP
        fnConst = (TypeApp .) . TypeApp . TypeSym . TypeSymP
        termP   = (typVarP <|> TypeSym <$> typSymP) `chainl1` return TypeApp
        typVarP = TypeVar . TypeVarP <$> identifier <?> "Type Variable"

-------------------------------------------------------------------------------

typSchP :: Parser (TypeSch ())
typSchP =
    pure (TypeSchP . (:[]) . TypeVarP)
        <*  reserved "forall"
        <*> identifier
        <*  reservedOp "=>"
        <*> typP
        <?> "Type (TypeSch Kind)"

-------------------------------------------------------------------------------

-- ... and that's it!

-------------------------------------------------------------------------------

-- III. Type Inference.

-------------------------------------------------------------------------------

-- Overview of the algorithm.

type TypeCheck a = StateT (Subst, Int) (Either Err) a

typeCheck :: Expr -> Either Err Expr
typeCheck =

    uncurry fmap 
         <<< const
         &&& flip runStateT ([], 0)
         . exprCheck prelude

-------------------------------------------------------------------------------

-- There are lots of errors that can happen when inferring types.

typErr = lift . Left . Err

uniErr :: (HasKind t, Show t, HasKind u, Show u) => 
          String -> t -> u -> TypeCheck a

uniErr msg t u = typErr $
    msg ++ "\n  "
        ++ show u ++ " (" ++ show (kind u) ++ ") and " 
        ++ show t ++ " (" ++ show (kind t) ++ ")"

-------------------------------------------------------------------------------

-- There are some new vocab words.

data Kind where

    Star :: Kind
    Kfun :: Kind -> Kind -> Kind

    deriving (Eq, Show)

-- Examples:
--     Int  :: *
--     List :: * -> *
--     Map  :: * -> * -> *

-------------------------------------------------------------------------------

-- The kinds for `Type a` data structures can be calculated recursively.

class HasKind t where
    kind :: t -> Kind

instance HasKind (Type Kind) where

    kind (TypeSym tc) = kind tc
    kind (TypeVar u)  = kind u
    kind (TypeApp (kind -> Kfun _ k) _) = k

instance HasKind (TypeVar Kind) where

    kind (TypeVarT k v) = k

instance HasKind (TypeSym Kind) where

    kind (TypeSymT k v) = k

-------------------------------------------------------------------------------

-- Some familiar types defined in out new vocabulary.  Tidy!

tString  = TypeSym (TypeSymT Star "String" )
tBool    = TypeSym (TypeSymT Star "Boolean")
tDouble  = TypeSym (TypeSymT Star "Double" )
tList    = TypeSym (TypeSymT (Kfun Star Star) "[]")
tArrow   = TypeSym (TypeSymT (Kfun Star (Kfun Star Star)) "->")

infixr 4 `fn`

fn :: Type Kind -> Type Kind -> Type Kind
fn a b = TypeApp (TypeApp tArrow a) b

-------------------------------------------------------------------------------

-- We need the ability to generate unique type variables 

newTypeVar :: Kind -> TypeCheck (Type Kind)
newTypeVar k = do
    (s, i) <- get
    put (s, i + 1)
    return (TypeVar (TypeVarT k ("tvar_" ++ show i)))

-------------------------------------------------------------------------------

-- Unification
-- -----------

-- Type substitutions

type Subst = [(TypeVar Kind, Type Kind)]

-------------------------------------------------------------------------------

-- Substitutions can be applied to types

class Substitute a where

    apply :: Subst -> a -> a
    getVars :: a -> [TypeVar Kind]

instance Substitute (Type Kind) where
    apply s (TypeVar (flip lookup s -> Just u)) = u
    apply _ (TypeVar u) = TypeVar u
    apply s (TypeApp l r) = TypeApp (apply s l) (apply s r)
    apply _ t = t
    
-- and it will be useful for calculating substitutions to have a way
-- to get the free `TyVar`s in a `Type`
    
    getVars (TypeVar u) = [u]
    getVars (TypeApp l r) = getVars l `L.union` getVars r
    getVars t = []

instance Substitute a => Substitute [a] where
    apply s = map (apply s)
    getVars = L.nub . concat . map getVars

instance Substitute Ass where
    apply s (i :>: sc) = i :>: (apply s sc)
    getVars (i :>: sc) = getVars sc

instance Substitute (TypeSch Kind) where
    apply s (TypeSchT ks qt) = TypeSchT ks (apply s qt)
    getVars (TypeSchT ks qt) = getVars qt

-------------------------------------------------------------------------------

-- It would be nice to have a simple tool for incrementally extending our
-- substitution environment.

ext :: Subst -> Subst -> Subst
ext sub1 sub2 = [ (u, apply sub1 t) | (u,t) <- sub2 ] ++ sub1

-------------------------------------------------------------------------------

-- M.ost G.eneral U.nifier modifies the substitution environment such that
-- both input types are the same.  This is the Principal Type;  further 
-- inference can only restrict this type.

mgu :: Type Kind -> Type Kind -> TypeCheck Subst

mgu (TypeApp f x) (TypeApp g y) = do 
    sub1 <- f `mgu` g
    sub2 <- apply sub1 x `mgu` apply sub1 y
    return (sub2 `ext` sub1)                           
                               
mgu (TypeVar u) t =
    u `varBind` t

mgu t (TypeVar u) =
    u `varBind` t

mgu (TypeSym t) (TypeSym u) | t == u =
    return []

mgu t u =
    uniErr "types do not unify" t u

-------------------------------------------------------------------------------

-- Once we've committed to creating a new substitution, we need to make
-- sure it's valid by (1) checking that the kinds match, and (2) checking
-- that we are not constructing a replacement for a type into itself.

varBind :: TypeVar Kind -> Type Kind -> TypeCheck Subst
varBind u t 
    | t == TypeVar u     = return []
    | u `elem` getVars t = uniErr "occurs check failed" t u  -- (2)
    | kind u /= kind t   = uniErr "kinds do not match"  t u  -- (1)
    | otherwise          = return [(u, t)]            

-------------------------------------------------------------------------------

-- With these tools in hand, we can implement the Unificiation step

unify :: Type Kind -> Type Kind -> TypeCheck ()
unify t1 t2 = do 
    s <- fst <$> get
    u <- apply s t1 `mgu` apply s t2
    get >>= return . first (ext u) >>= put

            
-------------------------------------------------------------------------------

-- Generalization
-- --------------

-- Consider the program

generalProg =

    "   let f = fun x -> x;               \
    \   f 1 == f 1                        \
    \       && f \"test\" == f \"test\"   "

-- Without generalization step, the application of `f 1` will introduce the
-- assumption `f : Num -> Num`, which makes the rest of the expression
-- invalid.

-- Generalization will guarantee that each application of `f` will have
-- new type variables.

-------------------------------------------------------------------------------

-- Generalizing types

quantify :: [TypeVar Kind] -> Type Kind -> TypeSch Kind
quantify vars typ = TypeSchT kinds (apply subs typ)
    where
        qVars = [ var | var <- getVars typ, var `elem` vars ]
        kinds = map kind qVars
        subs  = zip qVars (map TypeGen [ 0 .. ])

-------------------------------------------------------------------------------

-- With these tools, we can construct an environment aware `generalize`, which
-- applies the current substitution and only generalizes the free `TypeVar`s
-- in `valT`.

generalize :: [Ass] -> Type Kind -> TypeCheck (TypeSch Kind)
generalize as valT = do

    subs <- fst <$> get 
    return (quantify (getS subs valT L.\\ getS subs as) (apply subs valT))

    where
        getS x = (getVars .) . apply $ x

-------------------------------------------------------------------------------

-- ... and instantiating them again.

freshInst :: TypeSch Kind -> TypeCheck (Type Kind)
freshInst (TypeSchT ks qt) = do

    ts <- mapM newTypeVar ks
    return (inst ts qt)

    where
        inst ts (TypeApp l r) = TypeApp (inst ts l) (inst ts r)
        inst ts (TypeGen n) = ts !! n
        inst ts t = t

-------------------------------------------------------------------------------

-- Assumptions

data Ass = String :>: TypeSch Kind

prelude :: [Ass]
prelude =

    [ "==" :>: TypeSchT [Star] (TypeGen 0 `fn` TypeGen 0 `fn` tBool)
    , "&&" :>: TypeSchT [] (tBool `fn` tBool `fn` tBool)
    , "+"  :>: TypeSchT [] (tDouble `fn` tDouble `fn` tDouble)
    , "-"  :>: TypeSchT [] (tDouble `fn` tDouble `fn` tDouble) ]

find :: String -> [Ass] -> TypeCheck (TypeSch Kind)
find i [] = typErr ("unbound identifier: " ++ i)
find i ((i' :>: sc) : as) 
    | i == i'   = return sc
    | otherwise = find i as

-------------------------------------------------------------------------------

-- Inference
-- ---------

-- Literals

litCheck :: Lit -> Type Kind
litCheck (StrLit _)  = tString
litCheck (NumLit _)  = tDouble

-------------------------------------------------------------------------------

-- Patterns are a bit more complicated.

pattCheck :: [Ass] -> Patt -> TypeCheck ([Ass], Type Kind)

pattCheck as (ValPatt (LitVal l)) =
    return ([], litCheck l)

-- For starters, they can introduce bindings and hence `Ass`s.

pattCheck as (ValPatt (SymVal (Sym s))) = do
    t <- newTypeVar Star
    return ([ s :>: TypeSchT [] t ], t)

-------------------------------------------------------------------------------

pattCheck as (ValPatt (ConVal (TypeSym (TypeSymP l)))) = do
    sc <- find l as
    t  <- freshInst sc
    return ([], t)

-------------------------------------------------------------------------------

-- In order to check destruction patterns, we need to recreate the implied
-- (abstraction) type of the arguments, and unify with the constructor's
-- `Ass` from the environment.  

pattCheck as (ConPatt (TypeSymP con) ps) = do
    sc <- find con as
    x  <- sequence (map (pattCheck as) ps)
    t' <- newTypeVar Star
    t  <- freshInst sc
    unify t (foldr fn t' (map snd x))
    return (L.concat (map fst x), t')

-------------------------------------------------------------------------------

-- We need a way to get `Ass`s from `TypeSch ()`s

toAss :: TypeSym () -> TypeSch () -> [Ass]
toAss (TypeSymP name) (TypeSchP tvars typ) =

    [ name :>: quantify (toVar <$> tvars) (toType typ Star) ]

    where
 
-- Kind inference needs to be performed here - luckily this can be 
-- computed entirely from the AST's structure.

        toVar (TypeVarP n) = TypeVarT Star n

        toType (TypeSym (TypeSymP n)) k = TypeSym (TypeSymT k n)
        toType (TypeVar (TypeVarP n)) k = TypeVar (TypeVarT k n)
        toType (TypeApp f x) k =
            TypeApp (toType f (Kfun Star k)) (toType x Star)

-------------------------------------------------------------------------------

-- Expressions

exprCheck :: [Ass] -> Expr -> TypeCheck (Type Kind)

exprCheck as (VarExpr (LitVal l)) =
    return (litCheck l)

exprCheck as (LetExpr (TypBind typ typSch) expr) = do
    exprCheck (toAss typ typSch ++ as) expr

-------------------------------------------------------------------------------

exprCheck as (VarExpr (SymVal (Sym sym))) =
    find sym as >>= freshInst

exprCheck as (VarExpr (ConVal (TypeSym (TypeSymP sym)))) =
    find sym as >>= freshInst

--  x : σ ∈ Γ
--  ----------
--  Γ ⊦ x : σ

-------------------------------------------------------------------------------

-- TODO generalize me please!

exprCheck as (LetExpr (SymBind (Sym sym) val) expr) = do
    symT <- newTypeVar Star
    valT <- exprCheck ((sym :>: TypeSchT [] symT) : as) val
    unify valT symT
    schT <- generalize as valT 
    exprCheck (sym :>: schT : as) expr 

--  Γ ⊦ eₒ : σ   Γ, x : σ ⊦ e₁ : τ
--  ------------------------------
--  Γ ⊦ let x = eₒ in e₁ : τ

-------------------------------------------------------------------------------

exprCheck as (AppExpr f x) = do
    fT   <- exprCheck as f
    xT   <- exprCheck as x
    appT <- newTypeVar Star
    unify (xT `fn` appT) fT
    return appT

--  Γ ⊦ eₒ : τ → τ'   Γ ⊦ e₁ : τ
--  ----------------------------
--  Γ ⊦ eₒ e₁ : τ'

-------------------------------------------------------------------------------

exprCheck as (AbsExpr (Sym sym) expr) = do
    symT <- newTypeVar Star
    res  <- exprCheck ((sym :>: TypeSchT [] symT) : as) expr
    return (symT `fn` res)

--  Γ, x : τ ⊦ e : τ'
--  --------------------
--  Γ ⊦ λ x . e : τ → τ' 

-------------------------------------------------------------------------------

exprCheck as (MatExpr expr ((patt, res):[])) = do
    exprT <- exprCheck as expr
    (pattAs, pattT) <- pattCheck as patt
    unify exprT pattT
    exprCheck (pattAs ++ as) res

exprCheck as (MatExpr expr ((patt, res):es)) = do
    exprT <- exprCheck as expr
    (pattAs, pattT) <- pattCheck as patt
    unify exprT pattT
    resT  <- exprCheck (pattAs ++ as) res
    esT   <- exprCheck as (MatExpr expr es)
    unify resT esT
    return resT

-- IV. ------------------------------------------------------------------------

-- Code Generation
-- ===============

-- The Noisy Killer.

-------------------------------------------------------------------------------

-- Q: What is JMacro?

-- A: JMacro is a library for the programmatic generation of Javascript code.
--    It is designed to be multipurpose -- it is useful whether you are 
--    writing nearly vanilla Javascript or you are programmatically generating
--    Javascript either in an ad-hoc fashion or as the backend to a compiler
--    or EDSL. (1)

-- Sounds useful, if only we were in teh midst of writing a Javascript
-- compiler backend ...

-- (1) http://www.haskell.org/haskellwiki/Jmacro

-------------------------------------------------------------------------------

-- Features we can use without trying:

-- * Javascript AST and renderer.

--       TODO example

-- * Quasiquoted interface which also provides some new syntax, like
--   destructive bindings, optional haskell style syntax sugar.

-- * Hygienic names - this let's us declare new vars without 
--   knowledge of the environment.

-------------------------------------------------------------------------------

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
-- (3) HOLY SHIT WE'RE INSIDE JAVASCRIPT!
-- (4) HOLY SHIT WE JUST REFERENCED A HASKELL VALUE FROM INSIDE JAVASCRIPT!
-- (5) Ok, that was weird ...

-------------------------------------------------------------------------------

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
    toJExpr (ConVal (TypeSym (TypeSymP s))) = ref s

-- ... but the quasiquoter interface does not allow runtime variable names,
-- so for this and some other tasks we must construct the JExpr manually.

ref :: String -> JExpr
ref = ValExpr . JVar . StrI

-------------------------------------------------------------------------------

-- This enables us also to do one of the only manual processes neseccary -
-- at some point we'll need to introduce new variables into scope,
-- with a specific name and value.

intro :: (ToJExpr a) => String -> (JExpr -> a) -> Expr -> JExpr
intro sym f expr = [jmacroE| 

    function(arg) {                              // (1)
        `(DeclStat (StrI sym) Nothing)`;
        `(ref sym)` = `(f arg)`;                 // (2)
        return `(expr)`;
    }

|]

-- (1) `arg` is created in javascript ...
-- (2) ... but we can use it as an argument from Haskell?

-- GO HOME HASKELL, YOU'RE DRUNK

-------------------------------------------------------------------------------

instance ToJExpr Expr where

    toJExpr (VarExpr v) =

        toJExpr v

    toJExpr (AbsExpr (Sym sym) ex) = 

        intro sym id ex

    toJExpr (isInline -> Just (x, o, y)) =

        InfixExpr o (toJExpr x) (toJExpr y)

    toJExpr (AppExpr f x) = 

        [jmacroE| `(f)`(`(x)`) |]

-------------------------------------------------------------------------------

    toJExpr (LetExpr (SymBind (Sym sym) ex) expr) = [jmacroE| 

        `(intro sym (const ex) expr)`()

    |]

    toJExpr (LetExpr (TypBind (TypeSymP sym) typsch) expr) = [jmacroE|

        function() {
            var scheme = `(curriedFun sym typsch)`;
            return `(intro sym (const scheme) expr)`()
        }()

    |]

-------------------------------------------------------------------------------

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

    toJExpr (MatExpr val []) = [jmacroE|

        (function() {
            throw "Pattern Match Exhausted";
        })()

    |]


-------------------------------------------------------------------------------

isInline :: Expr -> Maybe (Expr, String, Expr)
isInline (AppExpr (AppExpr (VarExpr (SymVal (Sym o))) x) y) 
    | o `elem` concat ops  = Just (x, o, y)
isInline _ = Nothing

-------------------------------------------------------------------------------

curriedFun :: String -> TypeSch () -> JExpr

curriedFun sym (TypeSchP vars (TypeApp (TypeApp (TypeSym (TypeSymP "->")) _) fs)) = [jmacroE|

    function(x) {
        var args = [];
        `(args)`.push(x); 
        return `(curriedFun' sym args (TypeSchP vars fs))`;
    }

|]

curriedFun sym ts = curriedFun' sym "" ts

-------------------------------------------------------------------------------

curriedFun' sym args (TypeSchP vars (TypeApp (TypeApp (TypeSym (TypeSymP "->")) _) fs)) = [jmacroE|

    function(x) {
        `(args)`.push(x); 
        return `(curriedFun' sym args (TypeSchP vars fs))`;
    }

|]

curriedFun' sym args _ = [jmacroE| 

    new function() {
        this.attrs = `(args)`;
        this.type  = `(sym)`;
    }()

|]

-------------------------------------------------------------------------------

data Match = Match JExpr Patt JExpr deriving (Show)

instance ToJExpr Match where

    toJExpr (Match val (ValPatt (LitVal l)) _) =

        [jmacroE| `(l)` == `(val)` |]

    toJExpr (Match val (ValPatt (SymVal (Sym s))) scope) = [jmacroE|

        (function() {
            `(scope)`[`(s)`] = `(val)`;
            return true;
        })()

    |]

-------------------------------------------------------------------------------

    toJExpr (Match val (ValPatt (ConVal (TypeSym (TypeSymP s)))) scope) =

        [jmacroE| `(val)`.type == `(s)` |]

    toJExpr (Match val (ConPatt (TypeSymP sym) ps) scope) = [jmacroE|

        `(val)`.type == `(sym)` && (function() {

            var result = true;

            for (var arg in `(val)`.attrs) {
                var argg = `(val)`.attrs[arg];
                result = result && `(conds argg ps scope)`;
            }

            return result;

        })()

    |] where

        conds _ [] _ = [jmacroE| true |]
        conds val (x : xs) scope = [jmacroE|

            `(Match val x scope)` && `(conds val xs scope)`

        |]

-------------------------------------------------------------------------------

-- TODO show the examples working!

-------------------------------------------------------------------------------

-- THE END

-------------------------------------------------------------------------------


-- UTILS

toText :: JExpr  -> Either Err String
toText = Right . show . renderJs  

unwrap (Right x) = putStrLn x        >> putStrLn "----------------------"
unwrap (Left x)  = putStrLn (show x) >> putStrLn "----------------------"

deriving instance (Show a) => Show (Type a)  
deriving instance (Show a) => Show (TypeVar a)  
deriving instance (Show a) => Show (TypeSym a)  
deriving instance (Show a) => Show (TypeSch a)  

deriving instance (Eq a) => Eq (Type a)  
deriving instance (Eq a) => Eq (TypeVar a)  
deriving instance (Eq a) => Eq (TypeSym a)  
deriving instance (Eq a) => Eq (TypeSch a)

main :: IO ()
main = do

    source <- head <$> getArgs
    case compile source of
        Left  e  -> print e
        Right js -> putStrLn js




-- V. Wrap up, run the samples from the intro.

-- LICENSE
-- Copyright (c) 2013 Andrew Stein

-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTypeCheckES OF MERCHANTABILITY, FITNESS FOR A PARTypeCheckCULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTypeCheckON OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTypeCheckON WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

