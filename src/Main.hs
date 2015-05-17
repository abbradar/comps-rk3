{-# LANGUAGE DeriveTraversable, OverloadedStrings, LambdaCase #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Attoparsec.Text
import Control.Monad.Trans.State
import Debug.Trace

type VarName = Text

data Var = Temp Integer | Var VarName
         deriving (Show, Eq, Ord)

data Op1 = Ref | Deref | Neg
         deriving (Show, Eq, Ord)

data Op2 = Add | Mult | Subt | Div
         deriving (Show, Eq, Ord)

data RVal var = RVar var | RConst Integer
              deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data ComplexRHS var = Complex1 Op1 (RVal var)
                    | Complex2 (RVal var) Op2 (RVal var)
                    deriving (Show, Eq, Functor, Foldable, Traversable)

data RHS var = Simple (RVal var)
             | Complex (ComplexRHS var)
             deriving (Show, Eq, Functor, Foldable, Traversable)

data Statement' var = Stat var (RHS var)
                    deriving (Show, Eq, Functor, Foldable, Traversable)

type Statement = Statement' Var

type Source = [Statement]

var :: Parser Var
var =     Temp <$ string "_t" <*> decimal
      <|> Var <$> takeWhile1 isAlpha

op1 :: Parser Op1
op1 =     Ref <$ char '&'
      <|> Deref <$ char '*'
      <|> Neg <$ char '-'

op2 :: Parser Op2
op2 =     Add <$ char '+'
      <|> Mult <$ char '*'
      <|> Subt <$ char '-'
      <|> Div <$ char '/'

rval :: Parser (RVal Var)
rval =     RVar <$> var
       <|> RConst <$> decimal

complexRhs :: Parser (ComplexRHS Var)
complexRhs =     Complex1 <$> op1 <*> rval
             <|> Complex2 <$> rval <*> op2 <*> rval

rhs :: Parser (RHS Var)
rhs =     Complex <$> complexRhs
      <|> Simple <$> rval

statement :: Parser Statement
statement = Stat <$> var <* char '=' <*> rhs

source :: Parser Source
source = many $ statement <* endOfLine

class PrettyPprint a where
  pprint :: a -> String

instance PrettyPprint Var where
  pprint (Temp i) = "_t" ++ show i
  pprint (Var n) = T.unpack n

instance PrettyPprint a => PrettyPprint (RVal a) where
  pprint (RVar var) = pprint var
  pprint (RConst const) = show const

instance PrettyPprint Op1 where
  pprint Ref = "&"
  pprint Deref = "*"
  pprint Neg = "-"

instance PrettyPprint Op2 where
  pprint Add = "+"
  pprint Mult = "*"
  pprint Subt = "-"
  pprint Div = "/"

instance PrettyPprint a => PrettyPprint (ComplexRHS a) where
  pprint (Complex1 op a) = pprint op ++ " " ++ pprint a
  pprint (Complex2 a op b) = pprint a ++ " " ++ pprint op ++ " " ++ pprint b

instance PrettyPprint a => PrettyPprint (RHS a) where
  pprint (Simple a) = pprint a
  pprint (Complex a) = pprint a

instance PrettyPprint a => PrettyPprint (Statement' a) where
  pprint (Stat var rhs) = pprint var ++ " = " ++ pprint rhs

instance PrettyPprint a => PrettyPprint [a] where
  pprint = concat . map ((++ "\n") . pprint)

straighten :: Source -> Source
straighten src = concat $ flip evalState (0, M.empty) $ forM src $ \(Stat var rhs) -> do
  rhs' <- mapM rename rhs
  var' <- rename var
  case (var', rhs') of
   (Var _, (Complex _)) -> state $ \(n, renames) ->
     ([Stat (Temp n) rhs', Stat var' $ Simple $ RVar $ Temp n], (succ n, renames))
   _ -> return [Stat var' rhs']

  where rename (Temp i) = state $ \st@(n, renames) ->
          case M.lookup i renames of
           Nothing -> (Temp n, (succ n, M.insert i n renames))
           Just r -> (Temp r, st)
        rename a = return a

reduce :: Source -> Source
reduce = map $ \case
  Stat var (Complex (Complex2 (RConst a) op (RConst b))) ->
    Stat var $ Simple $ RConst $ case op of
                                  Add -> a + b
                                  Mult -> a * b
                                  Subt -> a - b
                                  Div -> a `div` b
  a -> a

-- We don't use *any* knowledge of operations when optimizing;
-- it can simply be redone other way though.
data Expression = EAtom (RVal Var)
                | EOp1 Op1 Expression
                | EOp2 Expression Op2 Expression
                deriving (Eq, Show, Ord)

merge :: Source -> Source
merge = flip evalState M.empty . optimize
  where optimize [] = return []
        optimize (c@(Stat var rhs):t) = do
          exp <- find rhs
          (c', t') <- case var of
            Temp _ -> gets $ \m -> case lookupTmp exp m of
              Just n' -> (Stat n' rhs, map (\(Stat var' rhs') -> Stat var' $ fmap (rename var n') rhs') t)
              Nothing -> (c, t)
            _ -> return (c, t)
          modify (M.insert var exp)
          (c':) <$> optimize t'

        lookupTmp exp = listToMaybe . sort . mapMaybe check . M.toList
          where check (n@(Temp _), exp') | exp == exp' = Just n
                check _ = Nothing

        rename from to n | n == from = to
        rename _ _ n = n

        find (Simple a) = lookup a
        find (Complex (Complex1 op a)) = EOp1 <$> pure op <*> lookup a
        find (Complex (Complex2 a op b)) = EOp2 <$> lookup a <*> pure op <*> lookup b

        lookup (RConst a) = return $ EAtom $ RConst a
        lookup r@(RVar n) = gets $ \m -> fromMaybe (EAtom r) $ M.lookup n m

optimize :: Source -> Source
optimize = straighten . merge . reduce . straighten

main :: IO ()
main = do
  inputs <- readLn
  forM_ [1..inputs] $ const $ do
    len <- readLn
    void getLine
    input <- T.pack <$> unlines <$> map (filter (/= ' ')) <$> mapM (const getLine) [1..len]
    void getLine
    src <- either fail return $ parseOnly (source <* endOfInput) input
    putStrLn $ pprint $ optimize src
